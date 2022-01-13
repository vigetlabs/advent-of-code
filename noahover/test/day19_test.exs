defmodule Day19Test do
  use ExUnit.Case
  doctest Day19

  import Day19

  test "day 19 part 1 example - count beacons" do
    answer =
      File.read!("input/day19_example.txt")
      |> parse_input()
      |> count_beacons()
    assert answer == 79
  end

  test "day 19 part 1 - count beacons" do
    answer =
      File.read!("input/day19.txt")
      |> parse_input()
      |> count_beacons()
    assert answer == 462
  end

  test "day 19 part 2 example - find largest manhattan distance" do
    answer =
      File.read!("input/day19_example.txt")
      |> parse_input()
      |> largest_manhattan_distance()
    assert answer == 3621
  end

  test "day 19 part 2 - find largest manhattan distance" do
    answer =
      File.read!("input/day19.txt")
      |> parse_input()
      |> largest_manhattan_distance()
    assert answer == 12158
  end
end
