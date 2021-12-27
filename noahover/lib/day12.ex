defmodule Day12 do
  @moduledoc """
  Advent of Code 2021 Day 12
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      x
      |> String.split("-", trim: true)
      |> List.to_tuple()
    end)
    |> Enum.reduce(%{}, fn {a, b}, acc ->
      old_a_list = Map.get(acc, a, [])
      new_acc = Map.put(acc, a, [b | old_a_list])
      old_b_list = Map.get(new_acc, b, [])
      Map.put(new_acc, b, [a | old_b_list])
    end)
  end

  @doc """
  Day 12 Part 1

  Count paths out of cave
  """
  def count_paths(paths_map) do
    count_paths_loop(paths_map, "start", [])
  end

  defp count_paths_loop(_paths_map, "end", _visited), do: 1
  defp count_paths_loop(paths_map, node, visited) do
    next_nodes = Map.get(paths_map, node, [])

    next_nodes
    |> Enum.map(fn x ->
      if String.downcase(x) == x and Enum.member?(visited, x) do
        0
      else
        count_paths_loop(paths_map, x, [node | visited])
      end
    end)
    |> Enum.sum()
  end

  @doc """
  Day 12 Part 1

  Count paths out of cave if you can visit one small cave more than once
  """
  def count_complicated_paths(paths_map) do
    count_complicated_paths_loop(paths_map, "start", [], false)
  end

  defp count_complicated_paths_loop(_paths_map, "end", _visited, _twice), do: 1
  defp count_complicated_paths_loop(paths_map, node, visited, twice) do
    next_nodes = Map.get(paths_map, node, [])

    next_nodes
    |> Enum.map(fn x ->
      cond do
        String.downcase(x) == x and Enum.member?(visited, x) and (twice or x == "start") -> 0
        String.downcase(x) == x and Enum.member?(visited, x) ->
          count_complicated_paths_loop(paths_map, x, [node | visited], true)

        true -> count_complicated_paths_loop(paths_map, x, [node | visited], twice)
      end
    end)
    |> Enum.sum()
  end
end
