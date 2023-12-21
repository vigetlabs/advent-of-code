defmodule Day4Test do
  use ExUnit.Case
  doctest Day4

  import Day4

  test "day 4 part 1 example - sum points" do
    answer =
      File.read!("input/day4_example.txt")
      |> parse_input()
      |> sum_points()

    assert answer == 13
  end

  test "day 4 part 1 - sum points" do
    answer =
      File.read!("input/day4.txt")
      |> parse_input()
      |> sum_points()

    assert answer == 21568
  end

  test "day 4 part 2 example - count card instances" do
    answer =
      File.read!("input/day4_example.txt")
      |> parse_input()
      |> count_card_instances()

    assert answer == 30
  end

  test "day 4 part 2 - count card instances" do
    answer =
      File.read!("input/day4.txt")
      |> parse_input()
      |> count_card_instances()

    assert answer == 11_827_296
  end
end
