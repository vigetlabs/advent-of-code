defmodule Day7 do
  @moduledoc """
  Advent of Code 2021 Day 7
  """

  def parse_input(input) do
    input
    |> String.trim_trailing("\n")
    |> String.split(",", trim: true)
    |> Enum.map(fn x -> String.to_integer(x) end)
  end

  @doc """
  Day 7 Part 1

  Find the most fuel efficient position for the crabs to align at
  """
  def optimal_crab_alignment(crabs) do
    min = Enum.min(crabs)
    max = Enum.max(crabs)

    min..max
    |> Enum.to_list()
    |> Enum.map(fn x -> calculate_fuel(crabs, x) end)
    |> Enum.min()
  end

  defp calculate_fuel([], _x), do: 0
  defp calculate_fuel([head | tail], x) do
    diff = cond do
      head > x -> head - x
      true -> x - head
    end
    diff + calculate_fuel(tail, x)
  end

  @doc """
  Day 7 Part 2

  Find the actual most fuel efficient position for the crabs to align at
  """
  def actual_optimal_crab_alignment(crabs) do
    min = Enum.min(crabs)
    max = Enum.max(crabs)

    min..max
    |> Enum.to_list()
    |> Enum.map(fn x -> actual_calculate_fuel(crabs, x) end)
    |> Enum.min()
  end

  defp actual_calculate_fuel([], _x), do: 0
  defp actual_calculate_fuel([head | tail], x) do
    diff = cond do
      head > x -> head - x
      true -> x - head
    end

    if diff > 0 do
      fuel =
        1..diff
        |> Enum.to_list()
        |> Enum.sum()

      fuel + actual_calculate_fuel(tail, x)
    else
      actual_calculate_fuel(tail, x)
    end
  end
end
