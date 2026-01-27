import gleam/bool.{guard}
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/charlist
import gleam/option.{type Option, None, Some}
import gleam/result

pub fn get(name: String) -> Option(String) {
  let var = charlist.from_string(name) |> getenv

  let as_bool = decode.run(var, decode.bool)
  use <- guard(when: result.is_ok(as_bool), return: None)

  Some(characters_to_binary(var))
}

@external(erlang, "os", "getenv")
fn getenv(env: charlist.Charlist) -> dynamic.Dynamic

@external(erlang, "unicode", "characters_to_binary")
fn characters_to_binary(chars: dynamic.Dynamic) -> String
