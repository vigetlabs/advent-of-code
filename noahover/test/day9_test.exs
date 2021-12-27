defmodule Day9Test do
  use ExUnit.Case
  doctest Day9

  import Day9

  test "day 9 part 1 example - low points" do
    answer =
      File.read!("input/day9_example.txt")
      |> parse_input()
      |> low_points()
    assert answer == 15
  end

  test "day 9 part 1 - low points" do
    answer =
      File.read!("input/day9.txt")
      |> parse_input()
      |> low_points()
    assert answer == 417
  end

  test "day 9 part 2 example - largest basins" do
    answer =
      File.read!("input/day9_example.txt")
      |> parse_input()
      |> largest_basins()
    assert answer == 1134
  end

  test "day 9 part 2 - largest basins" do
    answer =
      File.read!("input/day9.txt")
      |> parse_input()
      |> largest_basins()
    assert answer == 1148965
  end
end
