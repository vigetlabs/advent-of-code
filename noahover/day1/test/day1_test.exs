defmodule Day1Test do
  use ExUnit.Case
  doctest Day1

  import Day1

  @example_input """
  199
  200
  208
  210
  200
  207
  240
  269
  260
  263
  """

  test "day 1 part 1 example - counts increases" do
    answer =
      @example_input
      |> parse_input()
      |> count_increases()
    assert answer == 7
  end

  test "day 1 part 2 example - counts sum increases" do
    answer =
      @example_input
      |> parse_input()
      |> count_sum_increases()
    assert answer == 5
  end
end
