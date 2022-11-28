defmodule Day15 do
  @moduledoc """
  Advent of Code 2021 Day 15
  """

  def parse_input(input) do
    list =
      input
      |> String.split("\n", trim: true)
      |> Enum.map(fn x ->
        x
        |> String.graphemes()
        |> Enum.map(fn y -> String.to_integer(y) end)
      end)

    max = length(list)

    {max, to_point_map(list, 0, 0, max, %{})}
  end

  defp to_point_map(_list, max, _col, max, map), do: map
  defp to_point_map(list, row, max, max, map), do: to_point_map(list, row + 1, 0, max, map)
  defp to_point_map(list, row, col, max, map) do
    value =
      list
      |> Enum.at(row)
      |> Enum.at(col)

    path = cond do
      row == 0 and col == 0 -> 0
      true -> 10_000_000
    end

    new_map = Map.put(map, {row, col}, {value, path})
    to_point_map(list, row, col + 1, max, new_map)
  end

  @doc """
  Day 15 Part 1

  Find risk level of safest path
  """
  def safest_path({max, point_map}) do
    destination = {max - 1, max - 1}

    starting_point = Map.get(point_map, {0, 0})
    point_map = Map.delete(point_map, {0, 0})
    checked_map = %{{0, 0} => starting_point}

    dijkstra(checked_map, point_map, destination)
  end

  defp dijkstra(checked_map, point_map, destination) do
    current_node =
      checked_map
      |> Map.to_list()
      |> Enum.sort(fn {{r1, c1}, {v1, p1}}, {{r2, c2}, {v2, p2}} ->
        cond do
          p1 < p2 -> true
          p1 > p2 -> false
          true ->
            cond do
              r1 + c1 > r2 + c2 -> true
              r1 + c1 < r2 + c2 -> false
              true -> v1 <= v2
            end
        end
      end)
      |> List.first()

    case check_neighbors(checked_map, point_map, current_node, destination) do
      {:complete, path} -> path
      {:incomplete, new_checked_map, new_point_map} ->
        dijkstra(new_checked_map, new_point_map, destination)
    end
  end

  defp check_neighbors(_checked_map, _point_map, {{row, col}, {_value, path}}, {row, col}) do
    {:complete, path}
  end
  defp check_neighbors(checked_map, point_map, {{row, col} = key, {_value, path}}, _destination) do
    checked_map = Map.delete(checked_map, key)

    {new_checked_map, new_point_map} =
      {checked_map, point_map}
      |> maybe_update_point(row - 1, col, path)
      |> maybe_update_point(row + 1, col, path)
      |> maybe_update_point(row, col - 1, path)
      |> maybe_update_point(row, col + 1, path)

    {:incomplete, new_checked_map, new_point_map}
  end

  defp maybe_update_point({checked_map, point_map}, row, col, path) do
    case Map.get(point_map, {row, col}) do
      {value, 10_000_000} ->
        {Map.put(checked_map, {row, col}, {value, value + path}), Map.delete(point_map, {row, col})}
      _ -> {checked_map, point_map}
    end
  end

  @doc """
  Day 15 Part 2

  Find risk level of safest path for extended map
  """
  def extended_safest_path({max, point_map}) do
    point_map = extend_point_map(point_map, max)
    max = max * 5

    destination = {max - 1, max - 1}

    starting_point = Map.get(point_map, {0, 0})
    point_map = Map.delete(point_map, {0, 0})
    checked_map = %{{0, 0} => starting_point}

    dijkstra(checked_map, point_map, destination)
  end

  defp extend_point_map(point_map, max) do
    point_map
    |> extend_downward(max)
    |> extend_outward(max)
  end

  defp extend_downward(point_map, max) do
    point_map
    |> Map.to_list()
    |> extend_downward_loop(point_map, max)
  end

  defp extend_downward_loop([], point_map, _max), do: point_map
  defp extend_downward_loop([{{row, col}, {value, _path}} | tail], point_map, max) do
    new_point_map =
      point_map
      |> Map.put({max + row, col}, {new_value(value, 1), 10_000_000})
      |> Map.put({(2 * max) + row, col}, {new_value(value, 2), 10_000_000})
      |> Map.put({(3 * max) + row, col}, {new_value(value, 3), 10_000_000})
      |> Map.put({(4 * max) + row, col}, {new_value(value, 4), 10_000_000})

    extend_downward_loop(tail, new_point_map, max)
  end

  defp extend_outward(point_map, max) do
    point_map
    |> Map.to_list()
    |> extend_outward_loop(point_map, max)
  end

  defp extend_outward_loop([], point_map, _max), do: point_map
  defp extend_outward_loop([{{row, col}, {value, _path}} | tail], point_map, max) do
    new_point_map =
      point_map
      |> Map.put({row, max + col}, {new_value(value, 1), 10_000_000})
      |> Map.put({row, (2 * max) + col}, {new_value(value, 2), 10_000_000})
      |> Map.put({row, (3 * max) + col}, {new_value(value, 3), 10_000_000})
      |> Map.put({row, (4 * max) + col}, {new_value(value, 4), 10_000_000})

    extend_outward_loop(tail, new_point_map, max)
  end

  defp new_value(value, adder) do
    sum = value + adder
    cond do
      sum <= 9 -> sum
      true -> rem(sum, 9)
    end
  end
end
