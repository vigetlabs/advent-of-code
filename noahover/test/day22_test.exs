defmodule Day22Test do
  use ExUnit.Case
  doctest Day22

  import Day22

  test "day 22 part 1 small example - count on" do
    answer =
      File.read!("input/day22_small_example.txt")
      |> parse_input()
      |> count_on()
    assert answer == 39
  end

  test "day 22 part 1 large example - count on" do
    answer =
      File.read!("input/day22_large_example.txt")
      |> parse_input()
      |> count_on()
    assert answer == 590784
  end

  test "day 22 part 1 - count on" do
    answer =
      File.read!("input/day22.txt")
      |> parse_input()
      |> count_on()
    assert answer == 551693
  end

  test "day 22 part 1 extra large example - count on" do
    answer =
      File.read!("input/day22_extra_large_example.txt")
      |> parse_input()
      |> count_on()
    assert answer == 474140
  end

  test "day 22 part 2 extra large example - count all on everywhere" do
    answer =
      File.read!("input/day22_extra_large_example.txt")
      |> parse_input()
      |> count_everywhere()
    assert answer == 2758514936282235
  end

  test "day 22 part 2 - count all on everywhere" do
    answer =
      File.read!("input/day22.txt")
      |> parse_input()
      |> count_everywhere()
    assert answer == 1165737675582132
  end
end
