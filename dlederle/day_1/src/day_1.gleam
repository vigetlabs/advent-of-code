import gleam/list.{at, index_fold}
import gleam/result.{unwrap}
import gleam/io.{debug}

pub external type Posix

pub external fn read(filename) -> Result(content, error) = "Elixir.File" "read"

const max = 1_000_000_000 // I can't find the max integer size

pub fn main() {
  "input.txt"
  |> read()
  |> result.map(fn(el) {
    debug(el)
  })
}

pub fn count_increases(number_list: List(Int)) -> Int {
  index_fold(over: number_list, from: 0, with: fn(acc, _item, index) {
    let current = number_list |> at(index) |> unwrap(or: 0)
    let previous = number_list |> at(index - 1) |> unwrap(or: max)
    case current > previous {
      True -> acc + 1
      False -> acc
    }
  })
}
