defmodule Day6 do
  @moduledoc """
  Advent of Code 2021 Day 6
  """

  def parse_input(input) do
    input
    |> String.trim_trailing("\n")
    |> String.split(",", trim: true)
    |> Enum.map(fn x -> String.to_integer(x) end)
  end

  @doc """
  Day 6 Part 1

  Find number of fish after 80 days
  """
  def number_of_fish(fish) do
    fish
    |> simulate_80_days(0)
    |> length()
  end

  defp simulate_80_days(fish, 80), do: fish
  defp simulate_80_days(fish, day) do
    fish
    |> simulate_day()
    |> simulate_80_days(day + 1)
  end

  defp simulate_day([]), do: []
  defp simulate_day([head | tail]) do
    case head do
      0 -> [6 | [8 | simulate_day(tail)]]
      pos -> [(pos - 1) | simulate_day(tail)]
    end
  end

  @doc """
  Day 6 Part 2

  Find number of fish after 256 days
  """
  def number_of_fish_after_256(fish) do
    fish
    |> create_count_map()
    |> simulate_256_days(0)
    |> Map.to_list()
    |> Enum.reduce(0, fn {_k, v}, acc -> acc + v end)
  end

  defp create_count_map(fish) do
    %{}
    |> Map.put(0, Enum.count(fish, fn x -> x == 0 end))
    |> Map.put(1, Enum.count(fish, fn x -> x == 1 end))
    |> Map.put(2, Enum.count(fish, fn x -> x == 2 end))
    |> Map.put(3, Enum.count(fish, fn x -> x == 3 end))
    |> Map.put(4, Enum.count(fish, fn x -> x == 4 end))
    |> Map.put(5, Enum.count(fish, fn x -> x == 5 end))
    |> Map.put(6, Enum.count(fish, fn x -> x == 6 end))
    |> Map.put(7, Enum.count(fish, fn x -> x == 7 end))
    |> Map.put(8, Enum.count(fish, fn x -> x == 8 end))
  end

  defp simulate_256_days(count_map, 256), do: count_map
  defp simulate_256_days(count_map, day) do
    zero_count = Map.get(count_map, 0)

    %{}
    |> Map.put(0, Map.get(count_map, 1))
    |> Map.put(1, Map.get(count_map, 2))
    |> Map.put(2, Map.get(count_map, 3))
    |> Map.put(3, Map.get(count_map, 4))
    |> Map.put(4, Map.get(count_map, 5))
    |> Map.put(5, Map.get(count_map, 6))
    |> Map.put(6, zero_count + Map.get(count_map, 7))
    |> Map.put(7, Map.get(count_map, 8))
    |> Map.put(8, zero_count)
    |> simulate_256_days(day + 1)
  end
end
