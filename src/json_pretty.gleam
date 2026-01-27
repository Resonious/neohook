import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import gleam/string_tree.{type StringTree}
import termcolor

pub type JsonValue {
  JString(String)
  JNumber(Float)
  JBool(Bool)
  JNull
  JArray(List(JsonValue))
  JObject(Dict(String, JsonValue))
}

pub fn from_dynamic(d: Dynamic) -> Result(JsonValue, String) {
  case dynamic.classify(d) {
    "String" ->
      decode.run(d, decode.string)
      |> result.map(JString)
      |> result.map_error(fn(_) { "Failed to decode string" })

    "Int" ->
      decode.run(d, decode.int)
      |> result.map(fn(i) { JNumber(int.to_float(i)) })
      |> result.map_error(fn(_) { "Failed to decode int" })

    "Float" ->
      decode.run(d, decode.float)
      |> result.map(JNumber)
      |> result.map_error(fn(_) { "Failed to decode float" })

    "Bool" ->
      decode.run(d, decode.bool)
      |> result.map(JBool)
      |> result.map_error(fn(_) { "Failed to decode bool" })

    "List" -> {
      case decode.run(d, decode.list(decode.dynamic)) {
        Ok(items) -> {
          items
          |> list.map(from_dynamic)
          |> result.all
          |> result.map(JArray)
        }
        Error(_) -> Error("Failed to decode list")
      }
    }

    // For objects/dicts - try to decode as a dict
    _ -> {
      case decode.run(d, decode.dict(decode.string, decode.dynamic)) {
        Ok(dict_data) -> {
          dict_data
          |> dict.to_list
          |> list.map(fn(pair) {
            let #(key, value) = pair
            from_dynamic(value)
            |> result.map(fn(v) { #(key, v) })
          })
          |> result.all
          |> result.map(dict.from_list)
          |> result.map(JObject)
        }
        Error(_) -> Ok(JNull)
        // Default to null for unrecognized types
      }
    }
  }
}

// ANSI color codes
const color_reset = termcolor.reset

const color_string = termcolor.green

const color_number = termcolor.yellow

const color_bool = termcolor.magenta

const color_null = termcolor.gray

const color_key = termcolor.cyan

const color_bracket = termcolor.white

fn make_indent(level: Int) -> String {
  string.repeat("  ", level)
}

pub fn pretty_print(value: JsonValue) -> String {
  pretty_print_tree(value, 0)
  |> string_tree.to_string
}

pub fn pretty_print_tree(value: JsonValue, indent: Int) -> StringTree {
  case value {
    JString(s) ->
      string_tree.new()
      |> string_tree.append(color_string)
      |> string_tree.append("\"")
      |> string_tree.append(escape_string(s))
      |> string_tree.append("\"")
      |> string_tree.append(color_reset)

    JNumber(n) -> {
      let n_str = case n == int.to_float(float.truncate(n)) {
        True -> int.to_string(float.truncate(n))
        False -> float.to_string(n)
      }
      string_tree.new()
      |> string_tree.append(color_number)
      |> string_tree.append(n_str)
      |> string_tree.append(color_reset)
    }

    JBool(b) -> {
      let b_str = case b {
        True -> "true"
        False -> "false"
      }
      string_tree.new()
      |> string_tree.append(color_bool)
      |> string_tree.append(b_str)
      |> string_tree.append(color_reset)
    }

    JNull ->
      string_tree.new()
      |> string_tree.append(color_null)
      |> string_tree.append("null")
      |> string_tree.append(color_reset)

    JArray(items) -> {
      case items {
        [] ->
          string_tree.new()
          |> string_tree.append(color_bracket)
          |> string_tree.append("[]")
          |> string_tree.append(color_reset)
        _ -> {
          let next_indent = indent + 1
          let indent_str = make_indent(next_indent)
          let items_trees =
            items
            |> list.map(fn(item) {
              string_tree.from_string(indent_str)
              |> string_tree.append_tree(pretty_print_tree(item, next_indent))
            })
          let items_joined = join_trees(items_trees, ",\n")

          string_tree.new()
          |> string_tree.append(color_bracket)
          |> string_tree.append("[\n")
          |> string_tree.append(color_reset)
          |> string_tree.append_tree(items_joined)
          |> string_tree.append("\n")
          |> string_tree.append(make_indent(indent))
          |> string_tree.append(color_bracket)
          |> string_tree.append("]")
          |> string_tree.append(color_reset)
        }
      }
    }

    JObject(obj) -> {
      let entries = dict.to_list(obj)
      case entries {
        [] ->
          string_tree.new()
          |> string_tree.append(color_bracket)
          |> string_tree.append("{}")
          |> string_tree.append(color_reset)
        _ -> {
          let next_indent = indent + 1
          let indent_str = make_indent(next_indent)
          let entry_trees =
            entries
            |> list.map(fn(pair) {
              let #(key, val) = pair
              string_tree.from_string(indent_str)
              |> string_tree.append(color_key)
              |> string_tree.append("\"")
              |> string_tree.append(escape_string(key))
              |> string_tree.append("\"")
              |> string_tree.append(color_reset)
              |> string_tree.append(": ")
              |> string_tree.append_tree(pretty_print_tree(val, next_indent))
            })
          let entries_joined = join_trees(entry_trees, ",\n")

          string_tree.new()
          |> string_tree.append(color_bracket)
          |> string_tree.append("{\n")
          |> string_tree.append(color_reset)
          |> string_tree.append_tree(entries_joined)
          |> string_tree.append("\n")
          |> string_tree.append(make_indent(indent))
          |> string_tree.append(color_bracket)
          |> string_tree.append("}")
          |> string_tree.append(color_reset)
        }
      }
    }
  }
}

fn join_trees(trees: List(StringTree), separator: String) -> StringTree {
  case trees {
    [] -> string_tree.new()
    [first, ..rest] ->
      list.fold(rest, first, fn(acc, tree) {
        acc
        |> string_tree.append(separator)
        |> string_tree.append_tree(tree)
      })
  }
}

// Helper to escape special characters in strings
fn escape_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}

// Convenience function to parse JSON string and pretty print
pub fn parse_and_print(json_string: String) -> Result(String, String) {
  case json.parse(json_string, decode.dynamic) {
    Ok(dyn) -> {
      from_dynamic(dyn)
      |> result.map(pretty_print)
    }
    Error(_) -> Error("Failed to parse JSON")
  }
}
