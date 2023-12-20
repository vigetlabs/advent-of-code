defmodule Day3Test do
  use ExUnit.Case
  doctest Day3

  import Day3

  test "day 3 part 1 example - sum part numbers" do
    answer =
      File.read!("input/day3_example.txt")
      |> parse_input()
      |> sum_part_numbers()

    assert answer == 4361
  end

  test "day 3 part 1 - sum part numbers" do
    answer =
      File.read!("input/day3.txt")
      |> parse_input()
      |> sum_part_numbers()

    assert answer == 550_064
  end
end
