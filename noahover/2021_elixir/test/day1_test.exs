defmodule Day1Test do
  use ExUnit.Case
  doctest Day1

  import Day1

  test "day 1 part 1 example - counts increases" do
    answer =
      File.read!("input/day1_example.txt")
      |> parse_input()
      |> count_increases()
    assert answer == 7
  end

  test "day 1 part 1 - counts increases" do
    answer =
      File.read!("input/day1.txt")
      |> parse_input()
      |> count_increases()
    assert answer == 1616
  end

  test "day 1 part 2 example - counts sum increases" do
    answer =
      File.read!("input/day1_example.txt")
      |> parse_input()
      |> count_sum_increases()
    assert answer == 5
  end

  test "day 1 part 2 - counts sum increases" do
    answer =
      File.read!("input/day1.txt")
      |> parse_input()
      |> count_sum_increases()
    assert answer == 1645
  end
end
