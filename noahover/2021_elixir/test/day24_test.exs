defmodule Day24Test do
  use ExUnit.Case
  doctest Day24

  import Day24

  test "day 24 part 1 - highest valid value" do
    answer =
      File.read!("input/day24.txt")
      |> parse_input()
      |> highest_valid()
    assert answer == 51983999947999
  end

  test "day 24 part 2 - lowest valid value" do
    answer =
      File.read!("input/day24.txt")
      |> parse_input()
      |> lowest_valid()
    assert answer == 11211791111365
  end
end
