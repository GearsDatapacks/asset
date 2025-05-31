# Asset
**A tool for converting to Gleam's new `assert` syntax**

[![Package Version](https://img.shields.io/hexpm/v/asset)](https://hex.pm/packages/asset)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/asset/)

`asset` can be used to convert from `gleeunit`'s `should` module to Gleam's new
`assert` syntax, which is preferred as it provides more information about what
went wrong.

### Usage

To use `asset`, simply download the escript from the [release page](https://github.com/GearsDatapacks/asset/releases/tag/v1.1.0),
or add it to your project as a dev dependency, using `gleam add asset --dev`.
From there, you can run the CLI.

```sh
asset update
# Or, if you added `asset` as a dependency:
gleam run -m asset update
```

This will look for any `.gleam` files within the current directory or subdirectories,
and update any references to `gleeunit/should` functions into `assert` and
`let assert`. A different directory can be specified instead:

```sh
asset update test/
```

After using `asset` to update code, you should probably run `gleam format`. `asset`
tries to generate well formatted code, but its output is not 100% accurate to
the way the Gleam formatter formats code.

### Edge-cases

In specific codebases, such as the standard library (and probably only the standard
library), `gleeunit` cannot be used. In the standard library, this is because
`gleeunit` depends on `gleam_stdlib`, and that would create a dependency cycle.
Instead, the `should` module is essentially copied to a `gleam/should` module.
In that case, the `-m` flag can be used to change the default of `gleeunit/should`:

```sh
asset update -m gleam/should
```

`asset` does not perfectly handle all possibilities. While it should work for
most codebases, there could be some edge-cases where the transformations applied
are wrong. If you run into this, please open an issue and I will try to fix that
case. (Or just manually fix it in your codebase if it's not too much code)
