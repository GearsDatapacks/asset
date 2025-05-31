import argv
import filepath
import glance
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/pair
import gleam/result
import gleam/string
import glexer/token
import simplifile as file

const should_module = "gleeunit/should"

/// Call the `asset` CLI.
pub fn main() -> Nil {
  let argv = argv.load()

  case argv.arguments {
    ["update"] -> update(".", should_module)
    ["update", directory] -> update(directory, should_module)
    ["update", "-m", module] -> update(".", module)
    ["update", directory, "-m", module] -> update(directory, module)
    _ -> print_usage()
  }
}

fn update(directory: String, module: String) -> Nil {
  let files = collect_files(directory)
  list.each(files, fn(pair) { update_file(pair.0, pair.1, module) })
}

pub type Error {
  ParsingFailed(glance.Error)
  NoImportFound
}

/// Updates a single file to use `assert` syntax. Accepts the target module to
/// replace. `gleeunit/should` should be used for most cases.
pub fn update_text(
  contents: String,
  target_module: String,
) -> Result(String, Error) {
  case glance.module(contents) {
    Error(error) -> {
      Error(ParsingFailed(error))
    }
    Ok(module) -> {
      let info =
        ModuleInfo(
          import_alias: "",
          import_location: glance.Span(0, 0),
          option_import: None,
          src: contents,
        )

      case find_import(module.imports, target_module, info) {
        Error(_) -> Error(NoImportFound)
        Ok(import_) -> {
          let edits = find_edits(module.functions, import_)
          let result = apply_edits(contents, edits)
          Ok(result)
        }
      }
    }
  }
}

fn update_file(path: String, contents: String, target_module: String) -> Nil {
  case update_text(contents, target_module) {
    Error(NoImportFound) ->
      io.println_error(
        "No references to `" <> target_module <> "` found in " <> path,
      )
    Error(ParsingFailed(error)) ->
      io.println_error(
        "Failed to parse file " <> path <> ": " <> glance_error(error),
      )
    Ok(text) ->
      case file.write(path, text) {
        Error(error) -> print_error("write to file", path, error)
        Ok(_) -> io.println_error("Successfully updated " <> path)
      }
  }
}

type Edit {
  Edit(start: Int, end: Int, text: String)
}

fn find_edits(
  functions: List(glance.Definition(glance.Function)),
  info: ModuleInfo,
) -> List(Edit) {
  functions
  |> list.flat_map(fn(definition) {
    statements(definition.definition.body, info)
  })
  |> list.prepend(Edit(
    start: info.import_location.start,
    // Add 1 for the proceeding newline
    end: info.import_location.end + 1,
    text: "",
  ))
  |> list.sort(fn(a, b) {
    int.compare(a.start, b.start) |> order.break_tie(int.compare(a.end, b.end))
  })
}

fn statements(
  statements: List(glance.Statement),
  info: ModuleInfo,
) -> List(Edit) {
  list.flat_map(statements, statement(_, info))
}

fn expressions(
  expressions: List(glance.Expression),
  info: ModuleInfo,
  statement_start: Int,
) -> List(Edit) {
  list.flat_map(expressions, expression(_, info, statement_start))
}

fn statement(statement: glance.Statement, info: ModuleInfo) -> List(Edit) {
  case statement {
    glance.Assert(expression: value, message:, location:) ->
      case message {
        None -> expression(value, info, location.start)
        Some(message) -> expressions([value, message], info, location.start)
      }
    glance.Assignment(kind:, value:, location:, ..) ->
      case kind {
        glance.LetAssert(message: None) | glance.Let ->
          expression(value, info, location.start)
        glance.LetAssert(message: Some(message)) ->
          expressions([value, message], info, location.start)
      }
    glance.Expression(e) -> do_expression(e, info, e.location.start, Statement)
    glance.Use(function:, location:, ..) ->
      expression(function, info, location.start)
  }
}

fn optional(
  optional: Option(glance.Expression),
  info: ModuleInfo,
  statement_start: Int,
) -> List(Edit) {
  case optional {
    Some(e) -> expression(e, info, statement_start)
    None -> []
  }
}

fn fields(
  fields: List(glance.Field(glance.Expression)),
  info: ModuleInfo,
  statement_start: Int,
) -> List(Edit) {
  use field <- list.flat_map(fields)
  case field {
    glance.ShorthandField(..) -> []
    glance.UnlabelledField(item:) | glance.LabelledField(item:, ..) ->
      expression(item, info, statement_start)
  }
}

fn clauses(
  clauses: List(glance.Clause),
  info: ModuleInfo,
  statement_start: Int,
) -> List(Edit) {
  use clause <- list.flat_map(clauses)
  case clause.guard {
    None -> expression(clause.body, info, statement_start)
    Some(guard) -> expressions([clause.body, guard], info, statement_start)
  }
}

fn expression(
  expression: glance.Expression,
  info: ModuleInfo,
  statement_start: Int,
) -> List(Edit) {
  do_expression(expression, info, statement_start, Expression)
}

fn do_expression(
  ast: glance.Expression,
  info: ModuleInfo,
  statement_start: Int,
  position: AssertionPosition,
) -> List(Edit) {
  case ast {
    glance.BinaryOperator(location:, name:, left:, right:) ->
      case name {
        glance.Pipe ->
          pipe(left, right, location, info, statement_start, position)
        _ -> Error(Nil)
      }
      |> result.map(pair.second)
      |> result.lazy_unwrap(fn() {
        expressions([left, right], info, statement_start)
      })
    glance.BitString(..) -> []
    glance.Block(statements: body, ..) -> statements(body, info)
    glance.Call(location:, function:, arguments:) ->
      call(location, function, arguments, info, statement_start, position)
      |> result.map(pair.second)
      |> result.lazy_unwrap(fn() {
        list.append(
          expression(function, info, statement_start),
          fields(arguments, info, statement_start),
        )
      })
    glance.Case(subjects:, clauses: cs, ..) ->
      list.append(
        expressions(subjects, info, statement_start),
        clauses(cs, info, statement_start),
      )
    glance.Echo(expression: value, ..) -> optional(value, info, statement_start)
    glance.FieldAccess(container:, ..) ->
      expression(container, info, statement_start)
    glance.Float(..) -> []
    glance.Fn(body:, ..) -> statements(body, info)
    glance.FnCapture(function:, arguments_before:, arguments_after:, ..) ->
      list.flatten([
        expression(function, info, statement_start),
        fields(arguments_before, info, statement_start),
        fields(arguments_after, info, statement_start),
      ])
    glance.Int(..) -> []
    glance.List(elements:, rest:, ..) ->
      case rest {
        None -> expressions(elements, info, statement_start)
        Some(rest) -> expressions([rest, ..elements], info, statement_start)
      }
    glance.NegateBool(value:, ..) -> expression(value, info, statement_start)
    glance.NegateInt(value:, ..) -> expression(value, info, statement_start)
    glance.Panic(message:, ..) -> optional(message, info, statement_start)
    glance.RecordUpdate(record:, fields:, ..) ->
      expressions(
        [
          record,
          ..list.filter_map(fields, fn(field) {
            option.to_result(field.item, Nil)
          })
        ],
        info,
        statement_start,
      )
    glance.String(..) -> []
    glance.Todo(message:, ..) -> optional(message, info, statement_start)
    glance.Tuple(elements:, ..) -> expressions(elements, info, statement_start)
    glance.TupleIndex(tuple:, ..) -> expression(tuple, info, statement_start)
    glance.Variable(..) -> []
  }
}

fn pipe_args(
  left: glance.Expression,
  args: List(glance.Field(glance.Expression)),
) -> List(glance.Field(glance.Expression)) {
  [glance.UnlabelledField(left), ..args]
}

fn called_function(
  function: glance.Expression,
) -> Result(#(Option(String), String), Nil) {
  case function {
    glance.FieldAccess(
      container: glance.Variable(
        name: module,
        ..,
      ),
      label: name,
      ..,
    ) -> Ok(#(Some(module), name))
    glance.Variable(name:, ..) -> Ok(#(None, name))

    _ -> Error(Nil)
  }
}

fn pipe(
  left: glance.Expression,
  right: glance.Expression,
  location: glance.Span,
  info: ModuleInfo,
  statement_start: Int,
  position: AssertionPosition,
) -> Result(#(String, List(Edit)), Nil) {
  case right {
    glance.FieldAccess(
      container: glance.Variable(
        name: module,
        ..,
      ),
      label: name,
      ..,
    ) ->
      transform_assertion(
        Some(module),
        name,
        location,
        pipe_args(left, []),
        info,
        statement_start,
        position,
      )
    glance.Variable(name:, ..) ->
      transform_assertion(
        None,
        name,
        location,
        pipe_args(left, []),
        info,
        statement_start,
        position,
      )
    glance.Call(function:, arguments:, ..) ->
      function
      |> called_function
      |> result.try(fn(pair) {
        let #(module, name) = pair
        transform_assertion(
          module,
          name,
          location,
          pipe_args(left, arguments),
          info,
          statement_start,
          position,
        )
      })

    _ -> Error(Nil)
  }
}

fn call(
  location: glance.Span,
  function: glance.Expression,
  arguments: List(glance.Field(glance.Expression)),
  info: ModuleInfo,
  statement_start: Int,
  position: AssertionPosition,
) -> Result(#(String, List(Edit)), Nil) {
  function
  |> called_function
  |> result.try(fn(pair) {
    let #(module, name) = pair
    transform_assertion(
      module,
      name,
      location,
      arguments,
      info,
      statement_start,
      position,
    )
  })
}

type AssertionPosition {
  Statement
  Expression
  InsideAssertion
}

fn transform_assertion(
  module: Option(String),
  name: String,
  location: glance.Span,
  arguments: List(glance.Field(glance.Expression)),
  info: ModuleInfo,
  statement_start: Int,
  position: AssertionPosition,
) -> Result(#(String, List(Edit)), Nil) {
  case assertion_function(info, module, name) {
    Error(_) -> Error(Nil)
    Ok(BeError) ->
      transform_variant_check(
        arguments,
        location,
        "Error",
        info,
        statement_start,
        position,
      )
    Ok(BeFalse) ->
      transform_bool_check(
        arguments,
        location,
        False,
        info,
        position,
        statement_start,
      )
    Ok(BeNone) -> {
      case arguments {
        [glance.UnlabelledField(value)] -> {
          let precedence = glance.precedence(glance.Eq)
          let #(value, expr_edits) =
            maybe_wrap(value, info, precedence, statement_start)
          let #(none, import_edits) = option_constructor(NoneConstructor, info)

          let assert_ = "assert " <> value <> " == " <> none
          let #(value, edits) =
            replace_or_return(
              wrap_assert(assert_, position),
              location,
              position,
            )
          Ok(#(value, list.flatten([import_edits, expr_edits, edits])))
        }
        _ -> Error(Nil)
      }
    }
    Ok(BeOk) ->
      transform_variant_check(
        arguments,
        location,
        "Ok",
        info,
        statement_start,
        position,
      )
    Ok(BeSome) -> {
      let #(some, import_edits) = option_constructor(SomeConstructor, info)
      let result =
        transform_variant_check(
          arguments,
          location,
          some,
          info,
          statement_start,
          position,
        )
      result.map(result, fn(pair) {
        #(pair.0, list.append(import_edits, pair.1))
      })
    }
    Ok(BeTrue) ->
      transform_bool_check(
        arguments,
        location,
        True,
        info,
        position,
        statement_start,
      )
    Ok(Equal) ->
      transform_comparison(
        arguments,
        location,
        "==",
        info,
        position,
        statement_start,
      )
    Ok(NotEqual) ->
      transform_comparison(
        arguments,
        location,
        "!=",
        info,
        position,
        statement_start,
      )
    Ok(Fail) -> Ok(replace_or_return("panic", location, position))
  }
}

fn replace_or_return(
  text: String,
  location: glance.Span,
  position: AssertionPosition,
) -> #(String, List(Edit)) {
  case position {
    Expression | Statement -> #("", [Edit(location.start, location.end, text)])
    InsideAssertion -> #(text, [])
  }
}

fn wrap_assert(assert_: String, position: AssertionPosition) -> String {
  case position {
    Expression | InsideAssertion -> "{ " <> assert_ <> " }"
    Statement -> assert_
  }
}

type OptionConstructor {
  SomeConstructor
  NoneConstructor
}

fn option_constructor(
  constructor: OptionConstructor,
  info: ModuleInfo,
) -> #(String, List(Edit)) {
  let #(import_, edits) = case info.option_import {
    None -> {
      let import_ =
        OptionImport(alias: "option", some_name: None, none_name: None)
      #(import_, [Edit(0, 0, "import gleam/option\n")])
    }
    Some(i) -> #(i, [])
  }
  let name = case constructor {
    NoneConstructor ->
      case import_.none_name {
        None -> import_.alias <> ".None"
        Some(name) -> name
      }
    SomeConstructor ->
      case import_.some_name {
        None -> import_.alias <> ".Some"
        Some(name) -> name
      }
  }
  #(name, edits)
}

fn maybe_wrap(
  expression: glance.Expression,
  info: ModuleInfo,
  precedence: Int,
  statement_start: Int,
) -> #(String, List(Edit)) {
  let #(expr_src, edits) = get_src(expression, info, statement_start)
  let expr = case expression {
    glance.BinaryOperator(name:, ..) ->
      case glance.precedence(name) > precedence {
        True -> expr_src
        False -> "{ " <> expr_src <> " }"
      }
    _ -> expr_src
  }
  #(expr, edits)
}

fn get_src(
  expression: glance.Expression,
  info: ModuleInfo,
  statement_start: Int,
) -> #(String, List(Edit)) {
  case expression {
    glance.Variable(name:, ..) -> Ok(#(name, []))
    glance.BinaryOperator(name: glance.Pipe, left:, right:, location:) ->
      pipe(left, right, location, info, statement_start, InsideAssertion)
    glance.Call(location:, function:, arguments:) ->
      call(
        location,
        function,
        arguments,
        info,
        statement_start,
        InsideAssertion,
      )
    _ -> Error(Nil)
  }
  |> result.lazy_unwrap(fn() {
    #(slice(info.src, expression.location.start, expression.location.end), [])
  })
}

fn transform_comparison(
  arguments: List(glance.Field(glance.Expression)),
  location: glance.Span,
  operator: String,
  info: ModuleInfo,
  position: AssertionPosition,
  statement_start: Int,
) -> Result(#(String, List(Edit)), Nil) {
  let precedence = glance.precedence(glance.Eq)

  case arguments {
    [glance.UnlabelledField(left), glance.UnlabelledField(right)] -> {
      let #(left, left_edits) =
        maybe_wrap(left, info, precedence, statement_start)
      let #(right, right_edits) =
        maybe_wrap(right, info, precedence, statement_start)

      let assert_ = "assert " <> left <> " " <> operator <> " " <> right
      let #(value, edits) =
        replace_or_return(wrap_assert(assert_, position), location, position)
      Ok(#(value, list.flatten([left_edits, right_edits, edits])))
    }
    _ -> Error(Nil)
  }
}

fn transform_bool_check(
  arguments: List(glance.Field(glance.Expression)),
  location: glance.Span,
  check_for: Bool,
  info: ModuleInfo,
  position: AssertionPosition,
  statement_start: Int,
) -> Result(#(String, List(Edit)), Nil) {
  case arguments {
    [glance.UnlabelledField(value)] -> {
      let #(assert_, value_edits) = case check_for {
        False -> {
          let #(value, edits) = maybe_wrap(value, info, 100, statement_start)
          #("assert !" <> value, edits)
        }
        True -> {
          let #(value, edits) = get_src(value, info, statement_start)
          #("assert " <> value, edits)
        }
      }
      let #(value, edits) =
        replace_or_return(wrap_assert(assert_, position), location, position)
      Ok(#(value, list.append(value_edits, edits)))
    }
    _ -> Error(Nil)
  }
}

fn transform_variant_check(
  arguments: List(glance.Field(glance.Expression)),
  location: glance.Span,
  variant_name: String,
  info: ModuleInfo,
  statement_start: Int,
  position: AssertionPosition,
) -> Result(#(String, List(Edit)), Nil) {
  case arguments {
    [glance.UnlabelledField(value)] -> {
      let #(value, value_edits) = get_src(value, info, statement_start)
      let variable_name = case position {
        Expression | InsideAssertion -> "value"
        Statement -> "_"
      }

      let newline = case position {
        Expression | InsideAssertion -> {
          let indent = find_indent(info.src, statement_start)
          "\n" <> string.repeat(" ", indent)
        }
        Statement -> ""
      }

      let assignment =
        "let assert "
        <> variant_name
        <> "("
        <> variable_name
        <> ") = "
        <> value
        <> newline

      case position {
        Expression ->
          Ok(
            #(variable_name, [
              Edit(statement_start, statement_start, assignment),
              Edit(location.start, location.end, variable_name),
              ..value_edits
            ]),
          )
        Statement ->
          Ok(
            #(variable_name, [
              Edit(location.start, location.end, assignment),
              ..value_edits
            ]),
          )
        InsideAssertion ->
          Ok(
            #(variable_name, [
              Edit(statement_start, statement_start, assignment),
              ..value_edits
            ]),
          )
      }
    }
    _ -> Error(Nil)
  }
}

fn find_indent(src: String, position: Int) -> Int {
  let until_position = slice(src, 0, position)

  count_indentation(until_position, 0)
}

fn count_indentation(text: String, count: Int) -> Int {
  case string.ends_with(text, " ") {
    False -> count
    True -> count_indentation(string.drop_end(text, 1), count + 1)
  }
}

fn apply_edits(contents: String, edits: List(Edit)) -> String {
  list.fold(edits, #(contents, 0), fn(acc, edit) {
    let #(contents, offset) = acc
    let contents =
      replace_span(contents, edit.start + offset, edit.end + offset, edit.text)

    let edit_offset = string.byte_size(edit.text) + edit.start - edit.end
    let offset = offset + edit_offset
    #(contents, offset)
  }).0
}

fn replace_span(string: String, start: Int, end: Int, text: String) -> String {
  slice(string, 0, start) <> text <> slice(string, end, -1)
}

@external(javascript, "./asset_ffi.mjs", "slice")
fn slice(string: String, start: Int, end: Int) -> String {
  let length = case end {
    -1 -> string.byte_size(string) - start
    _ -> end - start
  }

  do_slice(string, start, length)
}

@external(erlang, "binary", "part")
fn do_slice(string: String, start: Int, length: Int) -> String

type ModuleInfo {
  ModuleInfo(
    import_alias: String,
    import_location: glance.Span,
    option_import: Option(OptionImport),
    src: String,
  )
}

type OptionImport {
  OptionImport(
    alias: String,
    some_name: Option(String),
    none_name: Option(String),
  )
}

type AssertionFunction {
  BeError
  BeFalse
  BeNone
  BeOk
  BeSome
  BeTrue
  Equal
  Fail
  NotEqual
}

fn assertion_function(
  info: ModuleInfo,
  module: Option(String),
  name: String,
) -> Result(AssertionFunction, Nil) {
  case module == Some(info.import_alias) {
    False -> Error(Nil)
    True ->
      case name {
        "be_error" -> Ok(BeError)
        "be_false" -> Ok(BeFalse)
        "be_none" -> Ok(BeNone)
        "be_ok" -> Ok(BeOk)
        "be_some" -> Ok(BeSome)
        "be_true" -> Ok(BeTrue)
        "equal" -> Ok(Equal)
        "fail" -> Ok(Fail)
        "not_equal" -> Ok(NotEqual)
        _ -> Error(Nil)
      }
  }
}

fn find_import(
  imports: List(glance.Definition(glance.Import)),
  module: String,
  info: ModuleInfo,
) -> Result(ModuleInfo, Nil) {
  case imports {
    [] ->
      case info.import_alias {
        "" -> Error(Nil)
        _ -> Ok(info)
      }
    [import_, ..imports] ->
      case import_.definition.module {
        "gleam/option" -> {
          let alias = case import_.definition.alias {
            Some(glance.Named(alias)) -> alias
            Some(glance.Discarded(_)) | None -> "option"
          }

          let some_name =
            import_.definition.unqualified_values
            |> list.find_map(fn(value) {
              case value.name {
                "Some" -> Ok(value.alias |> option.unwrap("Some"))
                _ -> Error(Nil)
              }
            })
            |> option.from_result

          let none_name =
            import_.definition.unqualified_values
            |> list.find_map(fn(value) {
              case value.name {
                "None" -> Ok(value.alias |> option.unwrap("None"))
                _ -> Error(Nil)
              }
            })
            |> option.from_result

          find_import(
            imports,
            module,
            ModuleInfo(
              ..info,
              option_import: Some(OptionImport(alias:, some_name:, none_name:)),
            ),
          )
        }

        m if m == module ->
          case import_.definition.alias {
            Some(glance.Named(alias)) ->
              find_import(
                imports,
                module,
                ModuleInfo(
                  ..info,
                  import_alias: alias,
                  import_location: import_.definition.location,
                ),
              )

            Some(glance.Discarded(_)) -> Error(Nil)
            None ->
              find_import(
                imports,
                module,
                ModuleInfo(
                  ..info,
                  import_alias: string.split(module, "/")
                    |> list.last
                    |> result.unwrap(""),
                  import_location: import_.definition.location,
                ),
              )
          }

        _ -> find_import(imports, module, info)
      }
  }
}

fn collect_files(directory: String) -> List(#(String, String)) {
  case file.read_directory(directory) {
    Error(error) -> {
      print_error("read directory", directory, error)
      []
    }
    Ok(items) ->
      list.flat_map(items, fn(item) {
        let path = filepath.join(directory, item)
        case file.is_directory(path) {
          Ok(True) -> collect_files(path)
          Ok(False) -> {
            case filepath.extension(path) {
              Ok("gleam") ->
                case file.read(path) {
                  Ok(contents) -> [#(path, contents)]
                  Error(error) -> {
                    print_error("read file", path, error)
                    []
                  }
                }
              _ -> []
            }
          }
          Error(error) -> {
            print_error("read file", path, error)
            []
          }
        }
      })
  }
}

fn print_error(action: String, path: String, error: file.FileError) -> Nil {
  io.println_error(
    "Failed to " <> action <> " " <> path <> ": " <> file.describe_error(error),
  )
}

fn print_usage() -> Nil {
  io.println(
    "Usage: asset update [directory]
  
Flags:
  -m <test module>  Changes the target module to replace, defaults to `gleeunit/should`.",
  )
}

fn glance_error(error: glance.Error) -> String {
  case error {
    glance.UnexpectedEndOfInput -> "Unexpected end of input"
    glance.UnexpectedToken(token:, position:) ->
      "Unexpected token at position "
      <> int.to_string(position.byte_offset)
      <> ": `"
      <> token.to_source(token)
      <> "`"
  }
}
