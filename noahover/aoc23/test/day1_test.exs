defmodule Day1Test do
  use ExUnit.Case
  doctest Day1

  import Day1

  test "day 1 part 1 example 1 - sum calibration values" do
    answer =
      File.read!("input/day1_example1.txt")
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

  test "day 1 part 2 example 2 - sum calibration values with words" do
    answer =
      File.read!("input/day1_example2.txt")
      |> parse_input()
      |> sum_calibration_values_with_words()

    assert answer == 281
  end

  test "day 1 part 2 - sum calibration values with words" do
    answer =
      File.read!("input/day1.txt")
      |> parse_input()
      |> sum_calibration_values_with_words()

    assert answer == 54265
  end
end
