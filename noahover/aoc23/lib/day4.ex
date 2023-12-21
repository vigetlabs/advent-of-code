defmodule Day4 do
  @moduledoc """
  Advent of Code 2023 Day 4
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      colon_split = String.split(x, ": ")

      id =
        colon_split
        |> hd()
        |> String.trim_leading("Card ")
        |> String.trim(" ")
        |> String.to_integer()

      bar_split =
        colon_split
        |> List.last()
        |> String.split(" | ")

      winning_numbers =
        bar_split
        |> hd()
        |> String.split(" ", trim: true)
        |> Enum.map(fn y -> String.to_integer(y) end)

      held_numbers =
        bar_split
        |> List.last()
        |> String.split(" ", trim: true)
        |> Enum.map(fn y -> String.to_integer(y) end)

      %{id: id, winning_numbers: winning_numbers, held_numbers: held_numbers}
    end)
  end

  @doc """
  Day 4 Part 1

  Sum the points of all the cards. You get points by
  having a number that is one of the winning numbers.
  The first matching number is worth 1 point and then
  each match after that doubles the score.
  """
  def sum_points(cards) do
    cards
    |> Enum.map(fn x -> to_points(x) end)
    |> Enum.sum()
  end

  defp to_points(%{winning_numbers: winning, held_numbers: held}) do
    held
    |> Enum.filter(fn x -> Enum.member?(winning, x) end)
    |> length()
    |> calculate_points()
  end

  defp calculate_points(0), do: 0

  defp calculate_points(i) do
    :math.pow(2, i - 1)
    |> round()
  end
end
