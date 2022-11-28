defmodule Day2 do
  @moduledoc """
  Advent of Code 2021 Day 2
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      [direction, distance_str] = String.split(x)
      {direction, String.to_integer(distance_str)}
    end)
  end

  @doc """
  Day 2 Part 1

  Follow directions to get product of final horizontal position and depth
  """
  def follow_directions([], horizontal_position, depth), do: horizontal_position * depth
  def follow_directions([{direction, distance} | tail], horizontal_position, depth) do
    case direction do
      "forward" -> follow_directions(tail, horizontal_position + distance, depth)
      "down" -> follow_directions(tail, horizontal_position, depth + distance)
      "up" -> follow_directions(tail, horizontal_position, depth - distance)
    end
  end

  @doc """
  Day 2 Part 2

  Follow more complicated directions to get product of final horizontal position and depth
  """
  def follow_complicated_directions([], horizontal_position, depth, _aim), do: horizontal_position * depth
  def follow_complicated_directions([{direction, distance} | tail], horizontal_position, depth, aim) do
    case direction do
      "forward" -> follow_complicated_directions(tail, horizontal_position + distance, depth + (aim * distance), aim)
      "down" -> follow_complicated_directions(tail, horizontal_position, depth, aim + distance)
      "up" -> follow_complicated_directions(tail, horizontal_position, depth, aim - distance)
    end
  end
end
