import gleam/io.{debug}
import gleam/result.{unwrap}
import gleam/string.{split}
import gleam/function.{compose}
import gleam/int
import gleam/bool
import gleam/list

pub external fn read_file(String) -> Result(String, a) = "file" "read_file"

pub type Location {
  Location(x: Int, y: Int)
}

pub type Command {
  Command(dir: Direction, magnitude: Int)
}

pub type Direction {
  Forward
  Down
  Up
}

pub fn main() -> Int {
  "input.txt"
  |> read_file()
  |> unwrap(or: "")
  |> split(on: "\n")
  |> list.filter(for: compose(string.is_empty, bool.negate))
  |> list.map(parse_command)
  |> list.fold(Location(x: 0, y: 0), apply_command)
  |> multiply_coords()
}

pub fn multiply_coords(loc: Location) -> Int {
  loc.x * loc.y
}

pub fn apply_command(from: Location, command: Command) -> Location {
  case command.dir {
    Forward -> Location(x: from.x + command.magnitude, y: from.y)
    Down -> Location(x: from.x, y: from.y + command.magnitude)
    Up -> Location(x: from.x, y: from.y - command.magnitude)
  }
}

pub fn parse_command(cmd: String) -> Command {
  let [dir, mag] = split(cmd, on: " ")
  let direction_result = parse_direction(dir)
  let Ok(magnitude_result) = int.parse(mag)

  Command(dir: direction_result, magnitude: magnitude_result)
}

// TODO: Handle errors and return Result lol
pub fn parse_direction(dir: String) -> Direction {
  case dir {
    "forward" -> Forward
    "up" -> Up
    "down" -> Down
  }
}
