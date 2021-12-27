defmodule Day5Test do
  use ExUnit.Case
  doctest Day5

  import Day5

  test "day 5 part 1 example - dangerous areas" do
    answer =
      File.read!("input/day5_example.txt")
      |> parse_input()
      |> most_dangerous_places()
    assert answer == 5
  end

  test "day 5 part 1 - dangerous areas" do
    answer =
      File.read!("input/day5.txt")
      |> parse_input()
      |> most_dangerous_places()
    assert answer == 5280
  end

  test "day 5 part 2 example - dangerous areas with diagonals" do
    answer =
      File.read!("input/day5_example.txt")
      |> parse_input()
      |> most_dangerous_places_with_diagonals()
    assert answer == 12
  end

  test "day 5 part 2 - dangerous areas with diagonals" do
    answer =
      File.read!("input/day5.txt")
      |> parse_input()
      |> most_dangerous_places_with_diagonals()
    assert answer == 16716
  end
end
