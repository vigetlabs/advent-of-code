defmodule Day8Test do
  use ExUnit.Case
  doctest Day8

  import Day8

  test "day 8 part 1 example - unique lengths" do
    answer =
      File.read!("input/day8_example.txt")
      |> parse_input()
      |> unique_lengths()
    assert answer == 26
  end

  test "day 8 part 1 - unique lengths" do
    answer =
      File.read!("input/day8.txt")
      |> parse_input()
      |> unique_lengths()
    assert answer == 255
  end

  test "day 8 part 2 example - sum decoded numbers" do
    answer =
      File.read!("input/day8_example.txt")
      |> parse_input()
      |> sum_decoded()
    assert answer == 61229
  end

  test "day 8 part 2 - sum decoded numbers" do
    answer =
      File.read!("input/day8.txt")
      |> parse_input()
      |> sum_decoded()
    assert answer == 982158
  end
end
