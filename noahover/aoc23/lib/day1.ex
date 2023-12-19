defmodule Day1 do
  @moduledoc """
  Advent of Code 2023 Day 1
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
  end

  @doc """
  Day 1 Part 1

  Sum the calibration values which are the first numeral in each line
  as the first digit and the last numeral in each line as the second digit.
  """
  def sum_calibration_values(rows) do
    rows
    |> Enum.map(fn x -> numerals_from_string(x) end)
    |> sum_from_numerals()
  end

  defp numerals_from_string(row) do
    row
    |> String.to_charlist()
    |> Enum.map(fn x -> x - 48 end)
    |> Enum.filter(fn x -> x >= 0 && x <= 9 end)
  end

  defp sum_from_numerals(numerals) do
    numerals
    |> Enum.map(fn x ->
      first_digit = hd(x)
      last_digit = List.last(x)

      first_digit * 10 + last_digit
    end)
    |> Enum.sum()
  end

  @doc """
  Day 1 Part 2

  Sum the calibration values which are the first numeral or spelled out
  numeral in each line as the first digit and the last numeral or spelled
  out numeral in each line as the second digit.
  """
  def sum_calibration_values_with_words(rows) do
    rows
    |> Enum.map(fn x -> words_to_calibration_value(x) end)
    |> Enum.sum()
  end

  defp words_to_calibration_value(row) do
    first_digit = find_first_digit(row)
    last_digit = find_last_digit(row)

    first_digit * 10 + last_digit
  end

  defp find_first_digit(row) do
    valid_digits = [
      "0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine"
    ]

    valid_digits
    |> Enum.reduce(%{}, fn x, acc ->
      case :binary.match(row, x) do
        {index, _length} ->
          Map.put(acc, x, index)

        _ ->
          acc
      end
    end)
    |> Map.to_list()
    |> Enum.sort(fn {_k1, v1}, {_k2, v2} -> v1 < v2 end)
    |> hd()
    |> elem(0)
    |> convert_text_to_number()
  end

  defp convert_text_to_number(s) do
    case s do
      "one" ->
        1

      "two" ->
        2

      "three" ->
        3

      "four" ->
        4

      "five" ->
        5

      "six" ->
        6

      "seven" ->
        7

      "eight" ->
        8

      "nine" ->
        9

      _ ->
        String.to_integer(s)
    end
  end

  defp find_last_digit(row) do
    valid_digits = [
      "0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "eno",
      "owt",
      "eerht",
      "ruof",
      "evif",
      "xis",
      "neves",
      "thgie",
      "enin"
    ]

    backwards_row = String.reverse(row)

    valid_digits
    |> Enum.reduce(%{}, fn x, acc ->
      case :binary.match(backwards_row, x) do
        {index, _length} ->
          Map.put(acc, x, index)

        _ ->
          acc
      end
    end)
    |> Map.to_list()
    |> Enum.sort(fn {_k1, v1}, {_k2, v2} -> v1 < v2 end)
    |> hd()
    |> elem(0)
    |> String.reverse()
    |> convert_text_to_number()
  end
end
