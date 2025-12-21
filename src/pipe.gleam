import gleam/result
import gleam/bit_array
import gleam/otp/actor
import ewe
import gleam/http

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

  Dead
}

pub fn new(kind: Kind) {
  actor.new(kind)
  |> actor.on_message(handle_message)
  |> actor.start
}

pub fn handle(
  state: Kind,
  message: Message,
) -> Kind {
  case message {
    PushEntry(entry) -> case state {
      Curl(body) -> {
        let res = ewe.send_chunk(body, entry.body)
          |> result.try(fn(_) { ewe.send_chunk(body, bit_array.from_string("\n")) })

        case res {
          Ok(Nil) -> state
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
