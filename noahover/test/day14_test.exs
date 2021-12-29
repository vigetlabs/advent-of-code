defmodule Day14Test do
  use ExUnit.Case
  doctest Day14

  import Day14

  test "day 14 part 1 example - difference between most and least after 10 steps" do
    answer =
      File.read!("input/day14_example.txt")
      |> parse_input()
      |> difference_after_10_steps()
    assert answer == 1588
  end

  test "day 14 part 1 - difference between most and least after 10 steps" do
    answer =
      File.read!("input/day14.txt")
      |> parse_input()
      |> difference_after_10_steps()
    assert answer == 3906
  end

  test "day 14 part 2 example - difference between most and least after 40 steps" do
    answer =
      File.read!("input/day14_example.txt")
      |> parse_input()
      |> difference_after_40_steps()
    assert answer == 2188189693529
  end

  test "day 14 part 2 - difference between most and least after 40 steps" do
    answer =
      File.read!("input/day14.txt")
      |> parse_input()
      |> difference_after_40_steps()
    assert answer == 4441317262452
  end
end
