import gleam/function
import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import gleam/bit_array
import gleam/otp/actor
import ewe
import gleam/http
import json_pretty

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
  Curl(ewe.ChunkedBody)
  Sse(ewe.SSEConnection)

  Dead
}

pub fn new(kind: Kind) {
  actor.new(kind)
  |> actor.on_message(handle_message)
  |> actor.start
}

fn ewe_send(body: ewe.ChunkedBody, contents: String) {
  ewe.send_chunk(body, bit_array.from_string(contents))
    |> result.try(fn(_) { Ok(body) })
}

pub fn handle(
  state: Kind,
  message: Message,
) -> Kind {
  case message {
    PushEntry(entry) -> case state {
      Curl(body) -> {
        let print_headers = fn(_) {
          list.try_each(
            over: entry.headers,
            with: fn(header) {
            let #(name, value) = header
            ewe_send(body, name)
            |> result.try(ewe_send(_, ": "))
            |> result.try(ewe_send(_, value))
            |> result.try(ewe_send(_, "\n"))
            }
          )
        }

        let string_to_send = json.parse_bits(entry.body, decode.dynamic)
        |> result.map_error(fn(_e) { "failed to decode" })
        |> result.try(json_pretty.from_dynamic)
        |> result.map(json_pretty.pretty_print)
        |> result.map(bit_array.from_string)
        |> result.lazy_unwrap(fn() { entry.body })

        let res = ewe_send(body, "\n")
        |> result.try(print_headers)
        |> result.try(fn(_) { ewe_send(body, "\n") })
        |> result.try(ewe.send_chunk(_, string_to_send))
        |> result.try(fn(_) { ewe_send(body, "\n") })

        case res {
          Ok(_) -> state
          Error(_) -> Dead
        }
      }

      Sse(conn) -> {
        let body_data = case bit_array.to_string(entry.body) {
          Ok(utf8) -> utf8
          Error(_) -> bit_array.base64_encode(entry.body, False)
        }

        let headers_data = dict.from_list(entry.headers)
          |> json.dict(function.identity, json.string)
          |> json.to_string

        let headers_event = ewe.event(headers_data) |> ewe.event_name("headers")
        let body_event = ewe.event(body_data)

        let res = ewe.send_event(conn, headers_event)
          |> result.map(fn(_) { conn })
          |> result.try(ewe.send_event(_, body_event))

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
