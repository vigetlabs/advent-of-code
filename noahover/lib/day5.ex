defmodule Day5 do
  @moduledoc """
  Advent of Code 2021 Day 5
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      x
      |> String.split(" -> ", trim: true)
      |> Enum.map(fn y ->
        y
        |> String.split(",", trim: true)
        |> Enum.map(fn z -> String.to_integer(z) end)
        |> List.to_tuple()
      end)
      |> List.to_tuple()
    end)
  end

  @doc """
  Day 5 Part 1

  Find places with overlapping lines
  """
  def most_dangerous_places(lines) do
    lines
    |> mark_points(%{})
    |> Map.to_list()
    |> Enum.map(fn {_k, v} -> v end)
    |> Enum.filter(fn x -> x > 1 end)
    |> length()
  end

  defp mark_points([], points), do: points
  defp mark_points([{{x1, y1}, {x2, y2}} | tail], points) do
    new_points = cond do
      x1 == x2 and y1 < y2 -> mark_vertical(points, x1, y1, y2)
      x1 == x2 -> mark_vertical(points, x1, y2, y1)
      y1 == y2 and x1 < x2 -> mark_horizontal(points, y1, x1, x2)
      y1 == y2 -> mark_horizontal(points, y1, x2, x1)
      true -> points
    end
    mark_points(tail, new_points)
  end

  defp mark_vertical(points, _x, y1, y2) when y1 > y2, do: points
  defp mark_vertical(points, x, y1, y2) do
    old_value = Map.get(points, {x, y1}, 0)
    points
    |> Map.put({x, y1}, old_value + 1)
    |> mark_vertical(x, y1 + 1, y2)
  end

  defp mark_horizontal(points, _y, x1, x2) when x1 > x2, do: points
  defp mark_horizontal(points, y, x1, x2) do
    old_value = Map.get(points, {x1, y}, 0)
    points
    |> Map.put({x1, y}, old_value + 1)
    |> mark_horizontal(y, x1 + 1, x2)
  end

  @doc """
  Day 5 Part 2

  Find places with overlapping lines including diagonals
  """
  def most_dangerous_places_with_diagonals(lines) do
    lines
    |> mark_points_with_diagonals(%{})
    |> Map.to_list()
    |> Enum.map(fn {_k, v} -> v end)
    |> Enum.filter(fn x -> x > 1 end)
    |> length()
  end

  defp mark_points_with_diagonals([], points), do: points
  defp mark_points_with_diagonals([{{x1, y1}, {x2, y2}} | tail], points) do
    new_points = cond do
      x1 == x2 and y1 < y2 -> mark_vertical(points, x1, y1, y2)
      x1 == x2 -> mark_vertical(points, x1, y2, y1)
      y1 == y2 and x1 < x2 -> mark_horizontal(points, y1, x1, x2)
      y1 == y2 -> mark_horizontal(points, y1, x2, x1)
      x1 < x2 and y1 < y2 -> mark_up_diagonal(points, x1, y1, x2)
      x1 < x2 -> mark_down_diagonal(points, x1, y1, x2)
      y1 < y2 -> mark_down_diagonal(points, x2, y2, x1)
      true -> mark_up_diagonal(points, x2, y2, x1)
    end
    mark_points_with_diagonals(tail, new_points)
  end

  defp mark_up_diagonal(points, x1, _y, x2) when x1 > x2, do: points
  defp mark_up_diagonal(points, x1, y, x2) do
    old_value = Map.get(points, {x1, y}, 0)
    points
    |> Map.put({x1, y}, old_value + 1)
    |> mark_up_diagonal(x1 + 1, y + 1, x2)
  end

  defp mark_down_diagonal(points, x1, _y1, x2) when x1 > x2, do: points
  defp mark_down_diagonal(points, x1, y, x2) do
    old_value = Map.get(points, {x1, y}, 0)
    points
    |> Map.put({x1, y}, old_value + 1)
    |> mark_down_diagonal(x1 + 1, y - 1, x2)
  end
end
