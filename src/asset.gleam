import argv
import filepath
import glance
import gleam/int
import gleam/io
import gleam/list
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
      Nil
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
