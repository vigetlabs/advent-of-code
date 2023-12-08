defmodule Day1Test do
  use ExUnit.Case
  doctest Day1

  import Day1

  test "day 1 part 1 example - sum calibration values" do
    answer =
      File.read!("input/day1_example.txt")
      |> parse_input()
      |> sum_calibration_values()

    assert answer == 142
  end

  test "day 1 part 1 - sum calibration values" do
    answer =
      File.read!("input/day1.txt")
      |> parse_input()
      |> sum_calibration_values()

    assert answer == 54450
  end
end
