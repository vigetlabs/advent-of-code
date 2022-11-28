defmodule Day12Test do
  use ExUnit.Case
  doctest Day12

  import Day12

  test "day 12 part 1 small example - count paths" do
    answer =
      File.read!("input/day12_small_example.txt")
      |> parse_input()
      |> count_paths()
    assert answer == 10
  end

  test "day 12 part 1 medium example - count paths" do
    answer =
      File.read!("input/day12_medium_example.txt")
      |> parse_input()
      |> count_paths()
    assert answer == 19
  end

  test "day 12 part 1 large example - count paths" do
    answer =
      File.read!("input/day12_large_example.txt")
      |> parse_input()
      |> count_paths()
    assert answer == 226
  end

  test "day 12 part 1 - count paths" do
    answer =
      File.read!("input/day12.txt")
      |> parse_input()
      |> count_paths()
    assert answer == 3292
  end

  test "day 12 part 2 small example - count complicated paths" do
    answer =
      File.read!("input/day12_small_example.txt")
      |> parse_input()
      |> count_complicated_paths()
    assert answer == 36
  end

  test "day 12 part 2 medium example - count complicated paths" do
    answer =
      File.read!("input/day12_medium_example.txt")
      |> parse_input()
      |> count_complicated_paths()
    assert answer == 103
  end

  test "day 12 part 2 large example - count complicated paths" do
    answer =
      File.read!("input/day12_large_example.txt")
      |> parse_input()
      |> count_complicated_paths()
    assert answer == 3509
  end

  test "day 12 part 2 - count complicated paths" do
    answer =
      File.read!("input/day12.txt")
      |> parse_input()
      |> count_complicated_paths()
    assert answer == 89592
  end
end
