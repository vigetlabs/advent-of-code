defmodule Day1 do
  @moduledoc """
  Advent of Code 2023 Day 1
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      x
      |> String.to_charlist()
      |> Enum.map(fn x -> x - 48 end)
      |> Enum.filter(fn x -> x >= 0 && x <= 9 end)
    end)
  end

  @doc """
  Day 1 Part 1

  Sum the calibration values which are the first numeral in each line
  as the first digit and the last numeral in each line as the second digit.
  """
  def sum_calibration_values(digits) do
    digits
    |> Enum.map(fn x ->
      first_digit = hd(x)
      last_digit = List.last(x)

      first_digit * 10 + last_digit
    end)
    |> Enum.sum()
  end
end
