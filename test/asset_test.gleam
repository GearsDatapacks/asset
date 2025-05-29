import gleeunit
import gleeunit/should

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  let name = "Joe"
  let greeting = Ok("Hello, " <> name <> "!")

  greeting
  |> should.be_ok
  |> should.equal("Hello, Joe!")
}
