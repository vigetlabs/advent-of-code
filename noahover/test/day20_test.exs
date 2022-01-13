defmodule Day20Test do
  use ExUnit.Case
  doctest Day20

  import Day20

  test "day 20 part 1 example - count lit pixels" do
    answer =
      File.read!("input/day20_example.txt")
      |> parse_input()
      |> count_lit()
    assert answer == 35
  end

  test "day 20 part 1 - count lit pixels" do
    answer =
      File.read!("input/day20.txt")
      |> parse_input()
      |> count_lit()
    assert answer == 5229
  end

  test "day 20 part 2 example - count lit pixels after 50 enhancements" do
    answer =
      File.read!("input/day20_example.txt")
      |> parse_input()
      |> count_lit_after_fifty()
    assert answer == 3351
  end

  test "day 20 part 2 - count lit pixels after 50 enhancements" do
    answer =
      File.read!("input/day20.txt")
      |> parse_input()
      |> count_lit_after_fifty()
    assert answer == 17009
  end
end
