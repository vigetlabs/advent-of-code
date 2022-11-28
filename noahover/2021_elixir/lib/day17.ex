defmodule Day17 do
  @moduledoc """
  Advent of Code 2021 Day 17
  """

  def parse_input(input) do
    input
    |> String.trim_trailing("\n")
    |> String.trim_leading("target area: ")
    |> String.split(", ", trim: true)
    |> Enum.map(fn x ->
      x
      |> String.slice(2, 100)
      |> String.split("..")
      |> Enum.map(fn y -> String.to_integer(y) end)
      |> List.to_tuple()
    end)
    |> List.to_tuple()
  end

  # Solved Day 17 Part 1 by hand, Answer: 2775

  @doc """
  Day 17 Part 2

  Find number of distinct initial velocities
  """
  def count_initial_velocities({{_x1, x2}, {y1, _y2}} = ranges) do
    min_y = y1
    max_y = -1 * y1
    min_x = 1
    max_x = x2 + 1

    count_initial_velocities_loop(min_x, min_y, min_y, max_x, max_y, ranges)
    |> length()
  end

  defp count_initial_velocities_loop(x, _y, _min_y, x, _max_y, _ranges), do: []
  defp count_initial_velocities_loop(x, y, min_y, max_x, y, ranges) do
    count_initial_velocities_loop(x + 1, min_y, min_y, max_x, y, ranges)
  end
  defp count_initial_velocities_loop(x, y, min_y, max_x, max_y, ranges) do
    case check_velocity(0, 0, x, y, ranges) do
      true -> [{x, y} | count_initial_velocities_loop(x, y + 1, min_y, max_x, max_y, ranges)]
      _ -> count_initial_velocities_loop(x, y + 1, min_y, max_x, max_y, ranges)
    end
  end

  defp check_velocity(x, y, vx, vy, {{x1, x2}, {y1, y2}} = ranges) do
    cond do
      x >= x1 and x <= x2 and y >= y1 and y <= y2 -> true
      x > x2 or y < y1 -> false
      true ->
        new_vx = case vx do
          0 -> 0
          _ -> vx - 1
        end
        check_velocity(x + vx, y + vy, new_vx, vy - 1, ranges)
    end
  end
end
