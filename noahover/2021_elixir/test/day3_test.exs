defmodule Day3Test do
  use ExUnit.Case
  doctest Day3

  import Day3

  test "day 3 part 1 example - power consumption" do
    answer =
      File.read!("input/day3_example.txt")
      |> parse_input()
      |> power_consumption()
    assert answer == 198
  end

  test "day 3 part 1 - power consumption" do
    answer =
      File.read!("input/day3.txt")
      |> parse_input()
      |> power_consumption()
    assert answer == 1307354
  end

  test "day 3 part 2 example - life support rating" do
    answer =
      File.read!("input/day3_example.txt")
      |> parse_input()
      |> life_support_rating()
    assert answer == 230
  end

  test "day 3 part 2 - life support rating" do
    answer =
      File.read!("input/day3.txt")
      |> parse_input()
      |> life_support_rating()
    assert answer == 482500
  end
end
