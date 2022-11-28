defmodule Day3 do
  @moduledoc """
  Advent of Code 2021 Day 3
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
  end

  @doc """
  Day 3 Part 1

  Check binary to determine power consumption
  """
  def power_consumption(diagnostics) do
    values =
      diagnostics
      |> Enum.map(fn x -> map_bits(x) end)
      |> Enum.reduce(fn x, acc ->
        Map.merge(acc, x, fn _k, v1, v2 -> v1 + v2 end)
      end)
      |> Map.to_list()
      |> Enum.map(fn {_k, v} -> v end)
    gr_bin = gamma_rate_binary(values)
    er_bin = epsilon_rate_binary(values)
    gamma_rate = String.to_integer(gr_bin, 2)
    epsilon_rate = String.to_integer(er_bin, 2)
    gamma_rate * epsilon_rate
  end

  defp map_bits(binary) do
    binary
    |> String.graphemes()
    |> Enum.map(fn x ->
      case x do
        "0" -> -1
        "1" -> 1
      end
    end)
    |> to_map(0, %{})
  end

  defp to_map([], _index, map), do: map
  defp to_map([head | tail], index, map) do
    to_map(tail, index + 1, Map.put(map, index, head))
  end

  defp gamma_rate_binary([]), do: ""
  defp gamma_rate_binary([head | tail]) do
    cond do
      head < 0 -> "0" <> gamma_rate_binary(tail)
      true -> "1" <> gamma_rate_binary(tail)
    end
  end

  defp epsilon_rate_binary([]), do: ""
  defp epsilon_rate_binary([head | tail]) do
    cond do
      head < 0 -> "1" <> epsilon_rate_binary(tail)
      true -> "0" <> epsilon_rate_binary(tail)
    end
  end

  @doc """
  Day 3 Part 2

  Check binary to determine life support rating
  """
  def life_support_rating(diagnostics) do
    ogr = oxygen_generator_rating(diagnostics, 0)
    csr = co2_scrubber_rating(diagnostics, 0)
    ogr * csr
  end

  defp oxygen_generator_rating([diagnostic], _bit), do: String.to_integer(diagnostic, 2)
  defp oxygen_generator_rating(diagnostics, bit) do
    count = count_bit(diagnostics, bit, 0)
    correct = cond do
      count < 0 -> "0"
      true -> "1"
    end

    diagnostics
    |> Enum.filter(fn x -> String.at(x, bit) == correct end)
    |> oxygen_generator_rating(bit + 1)
  end

  defp co2_scrubber_rating([diagnostic], _bit), do: String.to_integer(diagnostic, 2)
  defp co2_scrubber_rating(diagnostics, bit) do
    count = count_bit(diagnostics, bit, 0)
    correct = cond do
      count < 0 -> "1"
      true -> "0"
    end

    diagnostics
    |> Enum.filter(fn x -> String.at(x, bit) == correct end)
    |> co2_scrubber_rating(bit + 1)
  end

  defp count_bit([], _bit, count), do: count
  defp count_bit([head | tail], bit, count) do
    case String.at(head, bit) do
      "0" -> count_bit(tail, bit, count - 1)
      "1" -> count_bit(tail, bit, count + 1)
    end
  end
end
