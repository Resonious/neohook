import gleam/int
import gleam/result
import gleam/list
import gleam/option
import gleam/erlang/process
import gleam/dict
import gleam/otp/actor
import pipe

pub type Message {
  PushEntry(pipe_name: String, entry: pipe.Entry)

  AddPipe(
    name: String,
    id: Int,
    subject: process.Subject(pipe.Message),
  )

  CleanPipe(name: String, remove_id: Int)
}

pub type Subject = process.Subject(Message)

type State = dict.Dict(
  String,
  dict.Dict(Int, process.Subject(pipe.Message)),
)

pub fn new() {
  actor.new(dict.new())
  |> actor.on_message(handle_message)
  |> actor.start
}

pub fn new_pipe_id() -> Int {
  int.random(18446744073709551615)
}

fn handle_message(
  state: State,
  message: Message,
) -> actor.Next(State, Message) {
  case message {
    PushEntry(pipe_name, entry) -> {
      dict.get(state, pipe_name)
      |> result.lazy_unwrap(dict.new)
      |> dict.values
      |> list.each(process.send(_, pipe.PushEntry(entry)))

      actor.continue(state)
    }

    AddPipe(name, id, subject) -> {
      dict.combine(
        state,
        dict.from_list([#(name, dict.from_list([#(id, subject)]))]),
        dict.merge
      )
      |> actor.continue
    }

    CleanPipe(name, remove_id) -> {
      dict.upsert(
        state,
        name,
        fn(subjects) {
          subjects
          |> option.lazy_unwrap(dict.new)
          |> dict.delete(remove_id)
        },
      )
      |> actor.continue
    }
  }
}
