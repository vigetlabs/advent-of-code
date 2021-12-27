defmodule Day2Test do
  use ExUnit.Case
  doctest Day2

  import Day2

  test "day 2 part 1 example - product of horizontal position and depth" do
    answer =
      File.read!("input/day2_example.txt")
      |> parse_input()
      |> follow_directions(0, 0)
    assert answer == 150
  end

  test "day 2 part 1 - product of horizontal position and depth" do
    answer =
      File.read!("input/day2.txt")
      |> parse_input()
      |> follow_directions(0, 0)
    assert answer == 1694130
  end

  test "day 2 part 2 example - product of horizontal position and depth after more complicated directions" do
    answer =
      File.read!("input/day2_example.txt")
      |> parse_input()
      |> follow_complicated_directions(0, 0, 0)
    assert answer == 900
  end

  test "day 2 part 2 - product of horizontal position and depth after more complicated directions" do
    answer =
      File.read!("input/day2.txt")
      |> parse_input()
      |> follow_complicated_directions(0, 0, 0)
    assert answer == 1698850445
  end
end
