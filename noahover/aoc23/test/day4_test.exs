defmodule Day4Test do
  use ExUnit.Case
  doctest Day4

  import Day4

  test "day 4 part 1 example - sum points" do
    answer =
      File.read!("input/day4_example.txt")
      |> parse_input()
      |> sum_points()

    assert answer == 13
  end

  test "day 4 part 1 - sum points" do
    answer =
      File.read!("input/day4.txt")
      |> parse_input()
      |> sum_points()

    assert answer == 21568
  end
end
