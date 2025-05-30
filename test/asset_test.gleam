import asset
import birdie
import gleam/option.{type Option, None, Some}
import gleam/string
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

const template = "import <module><alias><other_import>

pub fn the_test() {
  <code>
}"

fn snap(code: String) {
  snap_with_options(
    code,
    Options(module: "gleeunit/should", alias: None, other_import: None),
  )
}

type Options {
  Options(module: String, alias: Option(String), other_import: Option(String))
}

fn snap_with_options(code: String, options: Options) {
  let alias = case options.alias {
    None -> ""
    Some(alias) -> " as " <> alias
  }

  let other_import = case options.other_import {
    None -> ""
    Some(module) -> "\nimport " <> module
  }

  let full_code =
    template
    |> string.replace("<code>", code)
    |> string.replace("<module>", options.module)
    |> string.replace("<alias>", alias)
    |> string.replace("<other_import>", other_import)

  let assert Ok(result) = asset.update_text(full_code, options.module)
    as "Update failed"

  "---- ORIGINAL CODE\n\n" <> full_code <> "\n\n---- AFTER UPDATE\n\n" <> result
}

pub fn equal_test() {
  "should.equal(x, y)"
  |> snap
  |> birdie.snap("equal")
}

pub fn not_equal_test() {
  "should.not_equal(x, y)"
  |> snap
  |> birdie.snap("not_equal")
}

pub fn be_ok_test() {
  "should.be_ok(result)"
  |> snap
  |> birdie.snap("be_ok")
}

pub fn be_error_test() {
  "should.be_error(result)"
  |> snap
  |> birdie.snap("be_error")
}

pub fn be_true_test() {
  "should.be_true(bool)"
  |> snap
  |> birdie.snap("be_true")
}

pub fn be_false_test() {
  "should.be_false(bool)"
  |> snap
  |> birdie.snap("be_false")
}

pub fn be_some_test() {
  "should.be_some(maybe)"
  |> snap
  |> birdie.snap("be_some")
}

pub fn be_none_test() {
  "should.be_none(maybe)"
  |> snap
  |> birdie.snap("be_none")
}

pub fn fail_test() {
  "should.fail()"
  |> snap
  |> birdie.snap("fail")
}
