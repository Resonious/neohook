import termcolor
import gleam/bool.{guard}
import gleam/bytes_tree
import gleam/erlang/process
import gleam/function
import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import gleam/bit_array
import gleam/otp/actor
import gleam/string_tree
import gleam/http
import json_pretty
import mist

pub type Entry {
  Entry(
    method: http.Method,
    headers: List(http.Header),
    body: BitArray,
  )
}

pub type Message {
  PushEntry(entry: Entry)
}

pub type Kind {
  Curl(process.Subject(bytes_tree.BytesTree), process.Pid)
  Sse(mist.SSEConnection)

  Dead
}

pub fn new(kind: Kind) {
  actor.new(kind)
  |> actor.on_message(handle_message)
  |> actor.start
}

fn send_sse_data(conn: mist.SSEConnection, data: String) -> Result(Nil, Nil) {
  mist.event(string_tree.from_string(data))
  |> mist.send_event(conn, _)
}

fn send_sse_named_event(conn: mist.SSEConnection, name: String, data: String) -> Result(Nil, Nil) {
  mist.event(string_tree.from_string(data))
  |> mist.event_name(name)
  |> mist.send_event(conn, _)
}

pub fn handle(
  state: Kind,
  message: Message,
) -> Kind {
  case message {
    PushEntry(entry) -> case state {
      Curl(subject, pid) -> {
        use <- guard(when: !process.is_alive(pid), return: Dead)

        let headers = list.map(
          entry.headers,
          fn(kv) {
            let #(name, value) = kv
            bytes_tree.new()
            |> bytes_tree.append(bit_array.from_string(termcolor.green))
            |> bytes_tree.append(bit_array.from_string(name))
            |> bytes_tree.append(bit_array.from_string(termcolor.reset))
            |> bytes_tree.append(bit_array.from_string(": "))
            |> bytes_tree.append(bit_array.from_string(termcolor.yellow))
            |> bytes_tree.append(bit_array.from_string(value))
            |> bytes_tree.append(bit_array.from_string(termcolor.reset))
            |> bytes_tree.append(bit_array.from_string("\n"))
          }
        )
        |> bytes_tree.concat

        let body = json.parse_bits(entry.body, decode.dynamic)
          |> result.map_error(fn(_e) { "failed to decode" })
          |> result.try(json_pretty.from_dynamic)
          |> result.map(json_pretty.pretty_print)
          |> result.map(bit_array.from_string)
          |> result.unwrap(entry.body)
          |> bytes_tree.from_bit_array

        let total = bytes_tree.from_string("\n")
          |> bytes_tree.append_tree(headers)
          |> bytes_tree.append_string("\n")
          |> bytes_tree.append_tree(body)
          |> bytes_tree.append_string("\n")

        process.send(subject, total)

        state
      }

      Sse(conn) -> {
        let body_data = case bit_array.to_string(entry.body) {
          Ok(utf8) -> utf8
          Error(_) -> bit_array.base64_encode(entry.body, False)
        }

        let headers_data = dict.from_list(entry.headers)
          |> json.dict(function.identity, json.string)
          |> json.to_string

        let res = send_sse_named_event(conn, "headers", headers_data)
          |> result.try(fn(_) { send_sse_data(conn, body_data) })

        case res {
          Ok(_) -> state
          Error(_) -> Dead
        }
      }

      Dead -> Dead
    }
  }
}

fn handle_message(
  state: Kind,
  message: Message,
) -> actor.Next(Kind, Message) {
  case handle(state, message) {
    Dead -> actor.stop()
    x -> actor.continue(x)
  }
}
