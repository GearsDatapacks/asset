import argv
import filepath
import glance
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glexer/token
import simplifile as file

pub fn main() -> Nil {
  let argv = argv.load()

  case argv.arguments {
    ["update"] -> update(".")
    ["update", directory] -> update(directory)
    _ -> print_usage(argv.program)
  }
}

fn update(directory: String) -> Nil {
  let files = collect_files(directory)
  list.each(files, fn(pair) { update_file(pair.0, pair.1) })
}

fn update_file(path: String, contents: String) -> Nil {
  case glance.module(contents) {
    Error(error) -> {
      io.println_error(
        "Failed to parse file " <> path <> ": " <> glance_error(error),
      )
    }
    Ok(module) -> {
      case find_import(contents, module.imports) {
        Error(_) -> Nil
        Ok(import_) -> {
          let edits = find_edits(module.functions, import_)
          let result = apply_edits(contents, edits)
          case file.write(path, result) {
            Error(error) -> print_error("write to file", path, error)
            Ok(_) -> Nil
          }
        }
      }
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
  |> list.sort(fn(a, b) { int.compare(a.start, b.start) })
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
) -> List(Edit) {
  list.flat_map(expressions, expression(_, info))
}

fn statement(statement: glance.Statement, info: ModuleInfo) -> List(Edit) {
  case statement {
    glance.Assert(expression: value, message:, ..) ->
      case message {
        None -> expression(value, info)
        Some(message) -> expressions([value, message], info)
      }
    glance.Assignment(kind:, value:, ..) ->
      case kind {
        glance.LetAssert(message: None) | glance.Let -> expression(value, info)
        glance.LetAssert(message: Some(message)) ->
          expressions([value, message], info)
      }
    glance.Expression(e) -> expression(e, info)
    glance.Use(function:, ..) -> expression(function, info)
  }
}

fn optional(optional: Option(glance.Expression), info: ModuleInfo) -> List(Edit) {
  case optional {
    Some(e) -> expression(e, info)
    None -> []
  }
}

fn fields(
  fields: List(glance.Field(glance.Expression)),
  info: ModuleInfo,
) -> List(Edit) {
  use field <- list.flat_map(fields)
  case field {
    glance.ShorthandField(..) -> []
    glance.UnlabelledField(item:) | glance.LabelledField(item:, ..) ->
      expression(item, info)
  }
}

fn clauses(clauses: List(glance.Clause), info: ModuleInfo) -> List(Edit) {
  use clause <- list.flat_map(clauses)
  case clause.guard {
    None -> expression(clause.body, info)
    Some(guard) -> expressions([clause.body, guard], info)
  }
}

fn expression(ast: glance.Expression, info: ModuleInfo) -> List(Edit) {
  case ast {
    glance.BinaryOperator(location:, name:, left:, right:) ->
      case name {
        glance.Pipe -> pipe(left, right, location, info)
        _ -> Error(Nil)
      }
      |> result.map(list.wrap)
      |> result.lazy_unwrap(fn() { expressions([left, right], info) })
    glance.BitString(..) -> []
    glance.Block(statements: body, ..) -> statements(body, info)
    glance.Call(location:, function:, arguments:) ->
      call(location, function, arguments, info)
    glance.Case(subjects:, clauses: cs, ..) ->
      list.append(expressions(subjects, info), clauses(cs, info))
    glance.Echo(expression: value, ..) -> optional(value, info)
    glance.FieldAccess(container:, ..) -> expression(container, info)
    glance.Float(..) -> []
    glance.Fn(body:, ..) -> statements(body, info)
    glance.FnCapture(function:, arguments_before:, arguments_after:, ..) ->
      list.flatten([
        expression(function, info),
        fields(arguments_before, info),
        fields(arguments_after, info),
      ])
    glance.Int(..) -> []
    glance.List(elements:, rest:, ..) ->
      case rest {
        None -> expressions(elements, info)
        Some(rest) -> expressions([rest, ..elements], info)
      }
    glance.NegateBool(value:, ..) -> expression(value, info)
    glance.NegateInt(value:, ..) -> expression(value, info)
    glance.Panic(message:, ..) -> optional(message, info)
    glance.RecordUpdate(record:, fields:, ..) ->
      expressions(
        [
          record,
          ..list.filter_map(fields, fn(field) {
            option.to_result(field.item, Nil)
          })
        ],
        info,
      )
    glance.String(..) -> []
    glance.Todo(message:, ..) -> optional(message, info)
    glance.Tuple(elements:, ..) -> expressions(elements, info)
    glance.TupleIndex(tuple:, ..) -> expression(tuple, info)
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
) -> Result(Edit, Nil) {
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
      )
    glance.Variable(name:, ..) ->
      transform_assertion(None, name, location, pipe_args(left, []), info)
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
) -> List(Edit) {
  function
  |> called_function
  |> result.try(fn(pair) {
    let #(module, name) = pair
    transform_assertion(module, name, location, arguments, info)
  })
  |> result.map(list.wrap)
  |> result.lazy_unwrap(fn() {
    list.append(expression(function, info), fields(arguments, info))
  })
}

fn transform_assertion(
  module: Option(String),
  name: String,
  location: glance.Span,
  arguments: List(glance.Field(glance.Expression)),
  info: ModuleInfo,
) -> Result(Edit, Nil) {
  case assertion_function(info, module, name) {
    Error(_) -> Error(Nil)
    Ok(BeError) -> Error(Nil)
    Ok(BeFalse) -> transform_bool_check(arguments, location, False, info)
    Ok(BeNone) -> Error(Nil)
    Ok(BeOk) -> Error(Nil)
    Ok(BeSome) -> Error(Nil)
    Ok(BeTrue) -> transform_bool_check(arguments, location, True, info)
    Ok(Equal) -> transform_comparison(arguments, location, "==", info)
    Ok(NotEqual) -> transform_comparison(arguments, location, "!=", info)
    Ok(Fail) -> Error(Nil)
  }
}

fn maybe_wrap(
  expression: glance.Expression,
  src: String,
  precedence: Int,
) -> String {
  let expr_src = slice(src, expression.location.start, expression.location.end)
  case expression {
    glance.BinaryOperator(name:, ..) ->
      case glance.precedence(name) > precedence {
        True -> expr_src
        False -> "{ " <> expr_src <> " }"
      }
    _ -> expr_src
  }
}

fn transform_comparison(
  arguments: List(glance.Field(glance.Expression)),
  location: glance.Span,
  operator: String,
  info: ModuleInfo,
) -> Result(Edit, Nil) {
  let precedence = glance.precedence(glance.Eq)

  case arguments {
    [glance.UnlabelledField(left), glance.UnlabelledField(right)] -> {
      let left = maybe_wrap(left, info.src, precedence)
      let right = maybe_wrap(right, info.src, precedence)

      let assert_ = "assert " <> left <> " " <> operator <> " " <> right
      Ok(Edit(location.start, location.end, assert_))
    }
    _ -> Error(Nil)
  }
}

fn transform_bool_check(
  arguments: List(glance.Field(glance.Expression)),
  location: glance.Span,
  check_for: Bool,
  info: ModuleInfo,
) -> Result(Edit, Nil) {
  case arguments {
    [glance.UnlabelledField(value)] -> {
      let assert_ = case check_for {
        False -> {
          let value = maybe_wrap(value, info.src, 100)
          "assert !" <> value
        }
        True -> {
          let value = slice(info.src, value.location.start, value.location.end)
          "assert " <> value
        }
      }
      Ok(Edit(location.start, location.end, assert_))
    }
    _ -> Error(Nil)
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
  ModuleInfo(import_alias: String, import_location: glance.Span, src: String)
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
  src: String,
  imports: List(glance.Definition(glance.Import)),
) -> Result(ModuleInfo, Nil) {
  list.find_map(imports, fn(import_) {
    case import_.definition.module {
      "gleeunit/should" ->
        case import_.definition.alias {
          Some(glance.Named(alias)) ->
            Ok(ModuleInfo(
              src:,
              import_alias: alias,
              import_location: import_.definition.location,
            ))
          Some(glance.Discarded(_)) -> Error(Nil)
          None ->
            Ok(ModuleInfo(
              src:,
              import_alias: "should",
              import_location: import_.definition.location,
            ))
        }

      _ -> Error(Nil)
    }
  })
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

fn print_usage(program: String) -> Nil {
  io.println("Usage: " <> program <> " update [directory]")
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
