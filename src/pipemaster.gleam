import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/result
import neohook/counter
import pipe

pub type Message {
  PushEntry(pipe_name: String, entry: pipe.Entry)

  AddPipe(name: String, id: Int, subject: process.Subject(pipe.Message))

  CleanPipe(name: String, remove_id: Int)
}

pub type Subject =
  process.Subject(Message)

type State =
  dict.Dict(String, dict.Dict(Int, process.Subject(pipe.Message)))

pub type Pipemaster =
  actor.Started(process.Subject(Message))

pub fn new(ctr: counter.Counter) -> Result(Pipemaster, actor.StartError) {
  actor.new(dict.new())
  |> actor.on_message(fn(x, y) { handle_message(ctr, x, y) })
  |> actor.start
}

pub fn new_pipe_id() -> Int {
  int.random(18_446_744_073_709_551_615)
}

fn handle_message(
  ctr: counter.Counter,
  state: State,
  message: Message,
) -> actor.Next(State, Message) {
  case message {
    PushEntry(pipe_name, entry) -> {
      dict.get(state, pipe_name)
      |> result.lazy_unwrap(dict.new)
      |> dict.values
      |> list.each(process.send(_, pipe.PushEntry(entry)))

      ctr.incr(pipe_name <> ":" <> "TODO: entry sender!")

      actor.continue(state)
    }

    AddPipe(name, id, subject) -> {
      dict.combine(
        state,
        dict.from_list([#(name, dict.from_list([#(id, subject)]))]),
        dict.merge,
      )
      |> actor.continue
    }

    CleanPipe(name, remove_id) -> {
      dict.upsert(state, name, fn(subjects) {
        subjects
        |> option.lazy_unwrap(dict.new)
        |> dict.delete(remove_id)
      })
      |> actor.continue
    }
  }
}
