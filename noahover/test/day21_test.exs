defmodule Day21Test do
  use ExUnit.Case
  doctest Day21

  import Day21

  test "day 21 part 1 example - score game" do
    answer =
      File.read!("input/day21_example.txt")
      |> parse_input()
      |> score_game()
    assert answer == 739785
  end

  test "day 21 part 1 - score game" do
    answer =
      File.read!("input/day21.txt")
      |> parse_input()
      |> score_game()
    assert answer == 752247
  end

  test "day 21 part 2 example - count universes" do
    answer =
      File.read!("input/day21_example.txt")
      |> parse_input()
      |> count_universes()
    assert answer == 444356092776315
  end

  test "day 21 part 2 - count universes" do
    answer =
      File.read!("input/day21.txt")
      |> parse_input()
      |> count_universes()
    assert answer == 221109915584112
  end
end
