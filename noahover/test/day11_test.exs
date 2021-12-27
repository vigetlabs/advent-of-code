defmodule Day11Test do
  use ExUnit.Case
  doctest Day11

  import Day11

  test "day 11 part 1 example - count octopus flashes" do
    answer =
      File.read!("input/day11_example.txt")
      |> parse_input()
      |> octopus_flashes()
    assert answer == 1656
  end

  test "day 11 part 1 - count octopus flashes" do
    answer =
      File.read!("input/day11.txt")
      |> parse_input()
      |> octopus_flashes()
    assert answer == 1608
  end

  test "day 11 part 2 example - find simultaneous flash" do
    answer =
      File.read!("input/day11_example.txt")
      |> parse_input()
      |> simultaneous_flash(1)
    assert answer == 195
  end

  test "day 11 part 2 - find simultaneous flash" do
    answer =
      File.read!("input/day11.txt")
      |> parse_input()
      |> simultaneous_flash(1)
    assert answer == 214
  end
end
