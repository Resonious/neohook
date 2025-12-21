import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string
import gleam/int
import gleam/float
import gleam/json

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
        Error(_) -> Ok(JNull)  // Default to null for unrecognized types
      }
    }
  }
}

// ANSI color codes
const color_reset = "\u{001b}[0m"
const color_string = "\u{001b}[32m"      // Green
const color_number = "\u{001b}[33m"      // Yellow
const color_bool = "\u{001b}[35m"        // Magenta
const color_null = "\u{001b}[90m"        // Gray
const color_key = "\u{001b}[36m"         // Cyan
const color_bracket = "\u{001b}[37m"     // White

fn make_indent(level: Int) -> String {
  string.repeat("  ", level)
}

pub fn pretty_print(value: JsonValue) -> String {
  pretty_print_internal(value, 0)
}

fn pretty_print_internal(value: JsonValue, indent: Int) -> String {
  case value {
    JString(s) -> color_string <> "\"" <> escape_string(s) <> "\"" <> color_reset
    
    JNumber(n) -> {
      // Check if it's a whole number
      let n_str = case n == int.to_float(float.truncate(n)) {
        True -> int.to_string(float.truncate(n))
        False -> float.to_string(n)
      }
      color_number <> n_str <> color_reset
    }
    
    JBool(b) -> {
      let b_str = case b {
        True -> "true"
        False -> "false"
      }
      color_bool <> b_str <> color_reset
    }
    
    JNull -> color_null <> "null" <> color_reset
    
    JArray(items) -> {
      case items {
        [] -> color_bracket <> "[]" <> color_reset
        _ -> {
          let next_indent = indent + 1
          let items_str = 
            items
            |> list.map(fn(item) {
              make_indent(next_indent) <> pretty_print_internal(item, next_indent)
            })
            |> string.join(",\n")
          
          color_bracket <> "[\n" <> color_reset 
            <> items_str <> "\n" 
            <> make_indent(indent) <> color_bracket <> "]" <> color_reset
        }
      }
    }
    
    JObject(obj) -> {
      let entries = dict.to_list(obj)
      case entries {
        [] -> color_bracket <> "{}" <> color_reset
        _ -> {
          let next_indent = indent + 1
          let entries_str =
            entries
            |> list.map(fn(pair) {
              let #(key, val) = pair
              make_indent(next_indent) 
                <> color_key <> "\"" <> escape_string(key) <> "\"" <> color_reset
                <> ": "
                <> pretty_print_internal(val, next_indent)
            })
            |> string.join(",\n")
          
          color_bracket <> "{\n" <> color_reset
            <> entries_str <> "\n"
            <> make_indent(indent) <> color_bracket <> "}" <> color_reset
        }
      }
    }
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
