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

const default_options = Options(
  module: "gleeunit/should",
  alias: None,
  other_import: None,
)

fn snap(code: String) {
  snap_with_options(code, default_options)
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

pub fn other_import_remains_test() {
  "should.equal(a, b)"
  |> snap_with_options(
    Options(..default_options, other_import: Some("some_other_module")),
  )
  |> birdie.snap("other_import_remains")
}

pub fn different_module_test() {
  "expect.equal(a, b)"
  |> snap_with_options(Options(..default_options, module: "glest/expect"))
  |> birdie.snap("different_module")
}

pub fn module_alias_test() {
  "expect.equal(a, b)"
  |> snap_with_options(Options(..default_options, alias: Some("expect")))
  |> birdie.snap("module_alias")
}

pub fn option_already_imported_test() {
  "should.be_some(thing)"
  |> snap_with_options(
    Options(..default_options, other_import: Some("gleam/option")),
  )
  |> birdie.snap("option_already_imported")
}

pub fn option_already_imported_aliased_test() {
  "should.be_some(thing)"
  |> snap_with_options(
    Options(..default_options, other_import: Some("gleam/option as maybe")),
  )
  |> birdie.snap("option_already_imported_aliased")
}

pub fn some_already_imported_unqualified_test() {
  "should.be_some(thing)"
  |> snap_with_options(
    Options(..default_options, other_import: Some("gleam/option.{Some}")),
  )
  |> birdie.snap("some_already_imported_unqualified")
}

pub fn none_already_imported_unqualified_test() {
  "should.be_none(thing)"
  |> snap_with_options(
    Options(..default_options, other_import: Some("gleam/option.{None}")),
  )
  |> birdie.snap("none_already_imported_unqualified")
}

pub fn some_already_imported_aliased_test() {
  "should.be_some(thing)"
  |> snap_with_options(
    Options(
      ..default_options,
      other_import: Some("gleam/option.{Some as Present}"),
    ),
  )
  |> birdie.snap("some_already_imported_aliased")
}

pub fn none_already_imported_aliased_test() {
  "should.be_none(thing)"
  |> snap_with_options(
    Options(
      ..default_options,
      other_import: Some("gleam/option.{None as Nothing}"),
    ),
  )
  |> birdie.snap("none_already_imported_aliased")
}

pub fn operator_precedence_test() {
  "should.equal(True && False, False)"
  |> snap
  |> birdie.snap("operator_precedence")
}

pub fn operator_precedence_high_enough_test() {
  "should.equal(1 + 2, 3)"
  |> snap
  |> birdie.snap("operator_precedence_high_enough")
}

pub fn operator_precedence_equal_test() {
  "should.equal(True == False, False)"
  |> snap
  |> birdie.snap("operator_precedence_equal")
}

pub fn transform_single_pipe_test() {
  "a |> transform |> should.equal(10)"
  |> snap
  |> birdie.snap("transform_single_pipe")
}

pub fn transform_single_pipe_call_test() {
  "a |> transform(10) |> should.equal(10)"
  |> snap
  |> birdie.snap("transform_single_pipe_call")
}

pub fn transform_single_pipe_call_labelled_test() {
  "a |> transform(10, thing: 20, other:) |> should.equal(10)"
  |> snap
  |> birdie.snap("transform_single_pipe_call_labelled")
}

pub fn do_not_transform_multi_pipe_test() {
  "a |> do_thing |> other_thing |> should.equal(10)"
  |> snap
  |> birdie.snap("do_not_transform_multi_pipe")
}

pub fn single_pipe_function_capture_test() {
  "a |> add(1, _) |> should.equal(3)"
  |> snap
  |> birdie.snap("single_pipe_function_capture")
}

pub fn single_pipe_function_capture_with_label_test() {
  "a |> add(left: 1, right: _) |> should.equal(3)"
  |> snap
  |> birdie.snap("single_pipe_function_capture_with_label")
}

pub fn single_pipe_single_precedence_test() {
  "a |> transform |> should.be_false"
  |> snap
  |> birdie.snap("single_pipe_single_precedence")
}

pub fn multi_pipe_precedence_test() {
  "a |> do_thing |> other_thing |> should.be_false"
  |> snap
  |> birdie.snap("multi_pipe_precedence")
}
