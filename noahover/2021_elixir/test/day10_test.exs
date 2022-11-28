defmodule Day10Test do
  use ExUnit.Case
  doctest Day10

  import Day10

  test "day 10 part 1 example - syntax error score" do
    answer =
      File.read!("input/day10_example.txt")
      |> parse_input()
      |> syntax_error_score()
    assert answer == 26397
  end

  test "day 10 part 1 - syntax error score" do
    answer =
      File.read!("input/day10.txt")
      |> parse_input()
      |> syntax_error_score()
    assert answer == 268845
  end

  test "day 10 part 2 example - incomplete syntax error score" do
    answer =
      File.read!("input/day10_example.txt")
      |> parse_input()
      |> incomplete_score()
    assert answer == 288957
  end

  test "day 10 part 2 - incomplete syntax error score" do
    answer =
      File.read!("input/day10.txt")
      |> parse_input()
      |> incomplete_score()
    assert answer == 4038824534
  end
end
