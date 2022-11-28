defmodule Day6Test do
  use ExUnit.Case
  doctest Day6

  import Day6

  test "day 6 part 1 example - number of fish" do
    answer =
      File.read!("input/day6_example.txt")
      |> parse_input()
      |> number_of_fish()
    assert answer == 5934
  end

  test "day 6 part 1 - number of fish" do
    answer =
      File.read!("input/day6.txt")
      |> parse_input()
      |> number_of_fish()
    assert answer == 346063
  end

  test "day 6 part 2 example - number of fish after 256 days" do
    answer =
      File.read!("input/day6_example.txt")
      |> parse_input()
      |> number_of_fish_after_256()
    assert answer == 26984457539
  end

  test "day 6 part 2 - number of fish after 256 days" do
    answer =
      File.read!("input/day6.txt")
      |> parse_input()
      |> number_of_fish_after_256()
    assert answer == 1572358335990
  end
end
