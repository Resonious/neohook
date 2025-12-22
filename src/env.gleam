import gleam/result
import gleam/bool.{guard}
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/charlist
import gleam/option.{type Option, Some, None}

pub fn get(name: String) -> Option(String) {
  let var = charlist.from_string(name) |> getenv

  let as_bool = decode.run(var, decode.bool)
  use <- guard(when: result.is_ok(as_bool), return: None)

  let as_charlist = decode.run(var, decode.list(of: decode.int))
  use <- guard(when: result.is_error(as_bool), return: None)
  let assert Ok(value) = as_charlist

  Some(characters_to_binary(value))
}

@external(erlang, "os", "getenv")
fn getenv(env: charlist.Charlist) -> dynamic.Dynamic

@external(erlang, "unicode", "characters_to_binary")
fn characters_to_binary(chars: List(Int)) -> String
