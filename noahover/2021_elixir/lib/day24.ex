defmodule Day24 do
  @moduledoc """
  Advent of Code 2021 Day 24
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.chunk_every(18)
    |> Enum.map(fn x -> convert_to_step(x) end)
    |> match_pairs(0)
  end

  defp convert_to_step(chunk) do
    direction_row = Enum.at(chunk, 4)

    down =
      chunk
      |> Enum.at(5)
      |> String.split(" ", trim: true)
      |> Enum.at(2)
      |> String.to_integer()

    up =
      chunk
      |> Enum.at(15)
      |> String.split(" ", trim: true)
      |> Enum.at(2)
      |> String.to_integer()

    case direction_row do
      "div z 1" -> {:up, up}
      "div z 26" -> {:down, -1 * down}
    end
  end

  defp match_pairs(_steps, 14), do: []
  defp match_pairs(steps, index) do
    {dir, sub} = Enum.at(steps, index)

    case dir do
      :down -> match_pairs(steps, index + 1)
      :up ->
        {dindex, val} = find_down(steps, index + 1, 0)
        [{index, dindex, val - sub} | match_pairs(steps, index + 1)]
    end
  end

  defp find_down(_steps, 14, _count), do: :invalid
  defp find_down(steps, index, 0) do
    case Enum.at(steps, index) do
      {:down, val} -> {index, val}
      {:up, _val} -> find_down(steps, index + 1, 1)
    end
  end
  defp find_down(steps, index, count) do
    case Enum.at(steps, index) do
      {:down, _val} -> find_down(steps, index + 1, count - 1)
      {:up, _val} -> find_down(steps, index + 1, count + 1)
    end
  end

  @doc """
  Day 24 Part 1

  Find highest valid number
  """
  def highest_valid(pairs) do
    pairs
    |> make_highest_digit_map(%{})
    |> Map.to_list()
    |> Enum.sort(fn {k1, _v1}, {k2, _v2} -> k1 <= k2 end)
    |> Enum.map(fn {_k, v} -> v end)
    |> Enum.join()
    |> String.to_integer()
  end

  defp make_highest_digit_map([], map), do: map
  defp make_highest_digit_map([{up, down, diff} | tail], map) do
    new_map = cond do
      diff < 0 ->
        map
        |> Map.put(down, 9)
        |> Map.put(up, 9 + diff)

      true ->
        map
        |> Map.put(up, 9)
        |> Map.put(down, 9 - diff)
    end

    make_highest_digit_map(tail, new_map)
  end

  @doc """
  Day 24 Part 2

  Find lowest valid number
  """
  def lowest_valid(pairs) do
    pairs
    |> make_lowest_digit_map(%{})
    |> Map.to_list()
    |> Enum.sort(fn {k1, _v1}, {k2, _v2} -> k1 <= k2 end)
    |> Enum.map(fn {_k, v} -> v end)
    |> Enum.join()
    |> String.to_integer()
  end

  defp make_lowest_digit_map([], map), do: map
  defp make_lowest_digit_map([{up, down, diff} | tail], map) do
    new_map = cond do
      diff < 0 ->
        map
        |> Map.put(up, 1)
        |> Map.put(down, 1 - diff)

      true ->
        map
        |> Map.put(down, 1)
        |> Map.put(up, 1 + diff)
    end

    make_lowest_digit_map(tail, new_map)
  end
end
