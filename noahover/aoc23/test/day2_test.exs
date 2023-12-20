defmodule Day2Test do
  use ExUnit.Case
  doctest Day2

  import Day2

  test "day 2 part 1 example 1 - sum possible game IDs" do
    answer =
      File.read!("input/day2_example.txt")
      |> parse_input()
      |> sum_possible_game_ids()

    assert answer == 8
  end

  test "day 2 part 1 - sum possible game IDs" do
    answer =
      File.read!("input/day2.txt")
      |> parse_input()
      |> sum_possible_game_ids()

    assert answer == 2278
  end

  test "day 2 part 2 example 1 - sum game powers" do
    answer =
      File.read!("input/day2_example.txt")
      |> parse_input()
      |> sum_game_powers()

    assert answer == 2286
  end

  test "day 2 part 2 - sum game powers" do
    answer =
      File.read!("input/day2.txt")
      |> parse_input()
      |> sum_game_powers()

    assert answer == 67953
  end
end
