defmodule Day7Test do
  use ExUnit.Case
  doctest Day7

  import Day7

  test "day 7 part 1 example - optimal crab position" do
    answer =
      File.read!("input/day7_example.txt")
      |> parse_input()
      |> optimal_crab_alignment()
    assert answer == 37
  end

  test "day 7 part 1 - optimal crab position" do
    answer =
      File.read!("input/day7.txt")
      |> parse_input()
      |> optimal_crab_alignment()
    assert answer == 364898
  end

  test "day 7 part 2 example - actual optimal crab position" do
    answer =
      File.read!("input/day7_example.txt")
      |> parse_input()
      |> actual_optimal_crab_alignment()
    assert answer == 168
  end

  test "day 7 part 2 - actual optimal crab position" do
    answer =
      File.read!("input/day7.txt")
      |> parse_input()
      |> actual_optimal_crab_alignment()
    assert answer == 104149091
  end
end
