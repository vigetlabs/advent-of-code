defmodule Day9 do
  @moduledoc """
  Advent of Code 2021 Day 9
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      x
      |> String.graphemes()
      |> Enum.map(fn y -> String.to_integer(y) end)
      |> to_map(0, %{})
    end)
    |> to_map(0, %{})
  end

  defp to_map([], _index, map), do: map
  defp to_map([head | tail], index, map) do
    to_map(tail, index + 1, Map.put(map, index, head))
  end

  @doc """
  Day 9 Part 1

  Count the low points
  """
  def low_points(matrix) do
    num_rows =
      matrix
      |> Map.keys()
      |> length()
    num_cols =
      matrix[0]
      |> Map.keys()
      |> length()

    matrix
    |> low_points_loop(0, 0, num_rows, num_cols)
    |> Enum.map(fn {r, c} -> matrix[r][c] + 1 end)
    |> Enum.sum()
  end

  defp low_points_loop(_matrix, num_rows, _col, num_rows, _num_cols), do: []
  defp low_points_loop(matrix, row, num_cols, num_rows, num_cols) do
    low_points_loop(matrix, row + 1, 0, num_rows, num_cols)
  end
  defp low_points_loop(matrix, row, col, num_rows, num_cols) do
    cond do
      low_point?(matrix, row, col, num_rows, num_cols) ->
        [{row, col} | low_points_loop(matrix, row, col + 1, num_rows, num_cols)]

      true -> low_points_loop(matrix, row, col + 1, num_rows, num_cols)
    end
  end

  defp low_point?(matrix, row, col, num_rows, num_cols) do
    value = matrix[row][col]

    lower?(value, matrix, row + 1, col, num_rows, num_cols)
      and lower?(value, matrix, row - 1, col, num_rows, num_cols)
      and lower?(value, matrix, row, col + 1, num_rows, num_cols)
      and lower?(value, matrix, row, col - 1, num_rows, num_cols)
  end

  defp lower?(_value, _matrix, -1, _col, _num_rows, _num_cols), do: true
  defp lower?(_value, _matrix, _row, -1, _num_rows, _num_cols), do: true
  defp lower?(_value, _matrix, num_rows, _col, num_rows, _num_cols), do: true
  defp lower?(_value, _matrix, _row, num_cols, _num_rows, num_cols), do: true
  defp lower?(value, matrix, row, col, _num_rows, _num_cols) do
    value < matrix[row][col]
  end

  @doc """
  Day 9 Part 2

  Get product of sizes of three largest basins
  """
  def largest_basins(matrix) do
    num_rows =
      matrix
      |> Map.keys()
      |> length()
    num_cols =
      matrix[0]
      |> Map.keys()
      |> length()

    matrix
    |> low_points_loop(0, 0, num_rows, num_cols)
    |> Enum.map(fn {r, c} ->
      matrix
      |> basin_points_set(r, c, num_rows, num_cols, -1)
      |> MapSet.size()
    end)
    |> Enum.sort(:desc)
    |> Enum.slice(0, 3)
    |> Enum.product()
  end

  defp basin_points_set(_matrix, -1, _col, _num_rows, _num_cols, _prev), do: MapSet.new()
  defp basin_points_set(_matrix, _row, -1, _num_rows, _num_cols, _prev), do: MapSet.new()
  defp basin_points_set(_matrix, num_rows, _col, num_rows, _num_cols, _prev), do: MapSet.new()
  defp basin_points_set(_matrix, _row, num_cols, _num_rows, num_cols, _prev), do: MapSet.new()
  defp basin_points_set(matrix, row, col, num_rows, num_cols, prev) do
    case matrix[row][col] do
      9 -> MapSet.new()
      i when i <= prev -> MapSet.new()
      val ->
        MapSet.new([{row, col}])
        |> MapSet.union(basin_points_set(matrix, row + 1, col, num_rows, num_cols, val))
        |> MapSet.union(basin_points_set(matrix, row - 1, col, num_rows, num_cols, val))
        |> MapSet.union(basin_points_set(matrix, row, col + 1, num_rows, num_cols, val))
        |> MapSet.union(basin_points_set(matrix, row, col - 1, num_rows, num_cols, val))
    end
  end
end
