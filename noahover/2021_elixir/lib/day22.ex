defmodule Day22 do
  @moduledoc """
  Advent of Code 2021 Day 22
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x -> convert_to_step(x) end)
  end

  defp convert_to_step(str) do
    {type_str, ranges_str} =
      str
      |> String.split(" ", trim: true)
      |> List.to_tuple()

    type = String.to_atom(type_str)

    {x, y, z} =
      ranges_str
      |> String.split(",", trim: true)
      |> Enum.map(fn x -> convert_to_range(x) end)
      |> List.to_tuple()

    {type, x, y, z}
  end

  defp convert_to_range(str) do
    {start, stop} =
      str
      |> String.slice(2, 100)
      |> String.split("..", trim: true)
      |> Enum.map(fn x -> String.to_integer(x) end)
      |> List.to_tuple()

    start..stop
  end

  @doc """
  Day 22 Part 1

  Count on cubes in initialization region
  """
  def count_on(steps) do
    steps
    |> count_on_loop(MapSet.new())
    |> MapSet.size()
  end

  defp count_on_loop([], set), do: set
  defp count_on_loop([{type, x_range, y_range, z_range} | tail], set) do
    points = points_from_ranges(x_range, y_range, z_range)

    new_list = case type do
      :on -> MapSet.union(set, points)
      :off -> MapSet.difference(set, points)
    end

    count_on_loop(tail, new_list)
  end

  defp points_from_ranges(xs..xe = x_range, ys..ye = y_range, zs..ze = z_range) do
    cond do
      xe < -50 or xs > 60 or ye < -50 or ys > 50 or ze < -50 or zs > 50 -> MapSet.new()
      xs < -50 -> points_from_ranges(-50..xe, y_range, z_range)
      ys < -50 -> points_from_ranges(x_range, -50..ye, z_range)
      zs < -50 -> points_from_ranges(x_range, y_range, -50..ze)
      xe > 50 -> points_from_ranges(xs..50, y_range, z_range)
      ye > 50 -> points_from_ranges(x_range, ys..50, z_range)
      ze > 50 -> points_from_ranges(x_range, y_range, zs..50)
      true ->
        points = for x <- x_range, y <- y_range, z <- z_range, do:  {x, y, z}
        MapSet.new(points)
    end
  end

  @doc """
  Day 22 Part 2

  Count on cubes everywhere
  """
  def count_everywhere(steps) do
    steps
    |> get_cubes([])
    |> sum_cubes(0)
  end

  defp get_cubes([], cubes), do: cubes
  defp get_cubes([head | tail], cubes), do: get_cubes(tail, add_cubes(cubes, head))

  defp add_cubes(cubes, {type, x, y, z}) do
    intersections =
      cubes
      |> Enum.map(fn cube -> intersection(cube, x, y, z) end)
      |> Enum.reject(fn cube -> cube == :empty end)

    new_cubes = case type do
      :on -> [{:add, x, y, z} | intersections]
      :off -> intersections
    end

    cubes ++ new_cubes
  end

  defp intersection({op, xs1..xe1, ys1..ye1, zs1..ze1}, xs2..xe2, ys2..ye2, zs2..ze2) do
    new_op = case op do
      :add -> :sub
      :sub -> :add
    end

    xs = max(xs1, xs2)
    ys = max(ys1, ys2)
    zs = max(zs1, zs2)

    xe = min(xe1, xe2)
    ye = min(ye1, ye2)
    ze = min(ze1, ze2)

    cond do
      xs > xe or ys > ye or zs > ze -> :empty
      true -> {new_op, xs..xe, ys..ye, zs..ze}
    end
  end

  defp sum_cubes([], sum), do: sum
  defp sum_cubes([{:add, x, y, z} | tail], sum) do
    new_sum = sum + cube_size(x, y, z)
    sum_cubes(tail, new_sum)
  end
  defp sum_cubes([{:sub, x, y, z} | tail], sum) do
    new_sum = sum - cube_size(x, y, z)
    sum_cubes(tail, new_sum)
  end

  defp cube_size(xs..xe, ys..ye, zs..ze), do: (xe - xs + 1) * (ye - ys + 1) * (ze - zs + 1)
end
