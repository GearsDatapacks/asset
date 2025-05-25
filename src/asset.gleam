import argv
import filepath
import glance
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
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
      case find_import(module.imports) {
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
  import_: ShouldImport,
) -> List(Edit) {
  functions
  |> list.flat_map(fn(definition) {
    statements(definition.definition.body, import_)
  })
  |> list.prepend(Edit(
    start: import_.location.start,
    // Add 1 for the proceeding newline
    end: import_.location.end + 1,
    text: "",
  ))
  |> list.sort(fn(a, b) { int.compare(a.start, b.start) })
}

fn statements(
  statements: List(glance.Statement),
  import_: ShouldImport,
) -> List(Edit) {
  list.flat_map(statements, statement(_, import_))
}

fn expressions(
  expressions: List(glance.Expression),
  import_: ShouldImport,
) -> List(Edit) {
  list.flat_map(expressions, expression(_, import_))
}

fn statement(statement: glance.Statement, import_: ShouldImport) -> List(Edit) {
  case statement {
    glance.Assert(expression: value, message:, ..) ->
      case message {
        None -> expression(value, import_)
        Some(message) -> expressions([value, message], import_)
      }
    glance.Assignment(kind:, value:, ..) ->
      case kind {
        glance.LetAssert(message: None) | glance.Let ->
          expression(value, import_)
        glance.LetAssert(message: Some(message)) ->
          expressions([value, message], import_)
      }
    glance.Expression(e) -> expression(e, import_)
    glance.Use(function:, ..) -> expression(function, import_)
  }
}

fn optional(
  optional: Option(glance.Expression),
  import_: ShouldImport,
) -> List(Edit) {
  case optional {
    Some(e) -> expression(e, import_)
    None -> []
  }
}

fn fields(
  fields: List(glance.Field(glance.Expression)),
  import_: ShouldImport,
) -> List(Edit) {
  use field <- list.flat_map(fields)
  case field {
    glance.ShorthandField(..) -> []
    glance.UnlabelledField(item:) | glance.LabelledField(item:, ..) ->
      expression(item, import_)
  }
}

fn clauses(clauses: List(glance.Clause), import_: ShouldImport) -> List(Edit) {
  use clause <- list.flat_map(clauses)
  case clause.guard {
    None -> expression(clause.body, import_)
    Some(guard) -> expressions([clause.body, guard], import_)
  }
}

fn expression(ast: glance.Expression, import_: ShouldImport) -> List(Edit) {
  case ast {
    glance.BinaryOperator(left:, right:, ..) ->
      expressions([left, right], import_)
    glance.BitString(..) -> []
    glance.Block(statements: body, ..) -> statements(body, import_)
    glance.Call(function:, arguments:, ..) ->
      list.append(expression(function, import_), fields(arguments, import_))
    glance.Case(subjects:, clauses: cs, ..) ->
      list.append(expressions(subjects, import_), clauses(cs, import_))
    glance.Echo(expression: value, ..) -> optional(value, import_)
    glance.FieldAccess(container:, ..) -> expression(container, import_)
    glance.Float(..) -> []
    glance.Fn(body:, ..) -> statements(body, import_)
    glance.FnCapture(function:, arguments_before:, arguments_after:, ..) ->
      list.flatten([
        expression(function, import_),
        fields(arguments_before, import_),
        fields(arguments_after, import_),
      ])
    glance.Int(..) -> []
    glance.List(elements:, rest:, ..) ->
      case rest {
        None -> expressions(elements, import_)
        Some(rest) -> expressions([rest, ..elements], import_)
      }
    glance.NegateBool(value:, ..) -> expression(value, import_)
    glance.NegateInt(value:, ..) -> expression(value, import_)
    glance.Panic(message:, ..) -> optional(message, import_)
    glance.RecordUpdate(record:, fields:, ..) ->
      expressions(
        [
          record,
          ..list.filter_map(fields, fn(field) {
            option.to_result(field.item, Nil)
          })
        ],
        import_,
      )
    glance.String(..) -> []
    glance.Todo(message:, ..) -> optional(message, import_)
    glance.Tuple(elements:, ..) -> expressions(elements, import_)
    glance.TupleIndex(tuple:, ..) -> expression(tuple, import_)
    glance.Variable(..) -> []
  }
}

fn apply_edits(contents: String, edits: List(Edit)) -> String {
  list.fold(edits, contents, fn(contents, edit) {
    replace_span(contents, edit.start, edit.end, edit.text)
  })
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

type ShouldImport {
  ShouldImport(alias: String, location: glance.Span)
}

fn is_assertion_function(
  import_: ShouldImport,
  module: Option(String),
  name: String,
) -> Bool {
  module == Some(import_.alias)
  && case name {
    "be_error"
    | "be_false"
    | "be_none"
    | "be_ok"
    | "be_some"
    | "be_true"
    | "equal"
    | "fail"
    | "not_equal" -> True
    _ -> False
  }
}

fn find_import(
  imports: List(glance.Definition(glance.Import)),
) -> Result(ShouldImport, Nil) {
  list.find_map(imports, fn(import_) {
    case import_.definition.module {
      "gleeunit/should" ->
        case import_.definition.alias {
          Some(glance.Named(alias)) ->
            Ok(ShouldImport(alias:, location: import_.definition.location))
          Some(glance.Discarded(_)) -> Error(Nil)
          None ->
            Ok(ShouldImport(
              alias: "should",
              location: import_.definition.location,
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
