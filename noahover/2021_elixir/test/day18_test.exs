defmodule Day18Test do
  use ExUnit.Case
  doctest Day18

  import Day18

  test "day 18 part 1 example - find magnitude of sum" do
    answer =
      File.read!("input/day18_example.txt")
      |> parse_input()
      |> find_magnitude()
    assert answer == 4140
  end

  test "day 18 part 1 - find magnitude of sum" do
    answer =
      File.read!("input/day18.txt")
      |> parse_input()
      |> find_magnitude()
    assert answer == 4057
  end

  test "day 18 part 2 example - find largest magnitude of sums between two numbers" do
    answer =
      File.read!("input/day18_example.txt")
      |> parse_input()
      |> find_largest_magnitude()
    assert answer == 3993
  end

  test "day 18 part 2 - find largest magnitude of sums between two numbers" do
    answer =
      File.read!("input/day18.txt")
      |> parse_input()
      |> find_largest_magnitude()
    assert answer == 4683
  end
end
