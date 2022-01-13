defmodule Day19 do
  @moduledoc """
  Advent of Code 2021 Day 19
  """

  def parse_input(input) do
    input
    |> String.split("\n\n", trim: true)
    |> Enum.map(fn x -> convert_to_scanner(x) end)
  end

  defp convert_to_scanner(input) do
    [_name | pos_strs] = String.split(input, "\n", trim: true)

    positions =
      pos_strs
      |> Enum.map(fn x ->
        x
        |> String.split(",", trim: true)
        |> Enum.map(fn y -> String.to_integer(y) end)
        |> List.to_tuple()
      end)

    Enum.reduce(positions, %{}, fn x, acc ->
      Map.put(acc, x, get_scanner_from_positions(x, positions))
    end)
  end

  defp get_scanner_from_positions(_pos, []), do: []
  defp get_scanner_from_positions({x1, y1, z1} = p1, [{x2, y2, z2} | tail]) do
    xd = x1 - x2
    yd = y1 - y2
    zd = z1 - z2

    sq_distance = (xd * xd) + (yd * yd) + (zd * zd)

    [sq_distance | get_scanner_from_positions(p1, tail)]
  end

  @doc """
  Day 19 Part 1

  Count the number of beacons
  """
  def count_beacons(scanners) do
    scanners
    |> unique_points()
    |> length()
  end

  defp unique_points(scanners) do
    unique =
      scanners
      |> Enum.map(fn x -> Map.to_list(x) end)
      |> List.flatten()

    len = length(scanners)

    remove_duplicates(scanners, unique, 0, 1, len, %{})
  end

  defp remove_duplicates(_scanners, unique, len, _two, len, _pairs_map), do: unique
  defp remove_duplicates(scanners, unique, one, len, len, pairs_map) do
    remove_duplicates(scanners, unique, one + 1, one + 2, len, pairs_map)
  end
  defp remove_duplicates(scanners, unique, one, two, len, pairs_map) do
    scanner1 = Enum.at(scanners, one)
    scanner2 = Enum.at(scanners, two)

    duplicates = find_duplicates(scanner1, scanner2)
    new_pairs_map = Enum.reduce(duplicates, pairs_map, fn {e1, e2}, acc ->
      list1 = [e2 | Map.get(acc, e1, [])]
      list2 = [e1 | Map.get(acc, e2, [])]
      combined = list1 ++ list2
      acc
      |> Map.put(e1, combined)
      |> Map.put(e2, combined)
    end)
    removed = Enum.map(duplicates, fn {e1, e2} ->
      case Enum.member?(unique, e2) do
        true -> e2
        false ->
          case Enum.member?(unique, e1) do
            true -> e1
            false ->
              new_pairs_map
              |> Map.get(e1)
              |> Enum.find(:invalid, fn x -> Enum.member?(unique, x) end)
          end
      end
    end)

    new_unique = cond do
      length(duplicates) < 12 -> unique
      true -> Enum.reject(unique, fn x -> Enum.member?(removed, x) end)
    end

    remove_duplicates(scanners, new_unique, one, two + 1, len, new_pairs_map)
  end

  defp find_duplicates(scanner1, scanner2) do
    scanner1_list = Map.to_list(scanner1)
    scanner2_list = Map.to_list(scanner2)

    find_duplicates_loop(scanner1_list, scanner2_list, scanner2_list)
  end

  defp find_duplicates_loop([], _scanner2_list, _original_scanner2_list), do: []
  defp find_duplicates_loop([_head | tail], [], original_scanner2_list) do
    find_duplicates_loop(tail, original_scanner2_list, original_scanner2_list)
  end
  defp find_duplicates_loop([{_p1, d1} = e1 | _t1] = l1, [{_p2, d2} = e2 | t2], original) do
    overlap_count = Enum.count(d2, fn x -> Enum.member?(d1, x) end)

    cond do
      overlap_count < 12 -> find_duplicates_loop(l1, t2, original)
      true -> [{e1, e2} | find_duplicates_loop(l1, t2, original)]
    end
  end

  @doc """
  Day 19 Part 2

  Find the largest Manhattan distance between scanners
  """
  def largest_manhattan_distance(scanners) do
    scanners
    |> scanners_points()
    |> manhattan_distances()
    |> Enum.max()
  end

  defp scanners_points(scanners) do
    len = length(scanners)

    with_points =
      scanners
      |> Enum.slice(0, 1)
      |> Enum.map(fn x -> {{0, 0, 0}, [], x} end)

    without_points = Enum.slice(scanners, 1, len)

    scanners_points_loop(with_points, without_points)
  end

  defp scanners_points_loop(with_points, without_points) do
    new_with_points = with_points ++ find_points(with_points, without_points, without_points)
    scanners = Enum.map(new_with_points, fn {_p, _m, s} -> s end)
    new_without_points = Enum.reject(without_points, fn x -> Enum.member?(scanners, x) end)

    case length(new_without_points) > 0 do
      true -> scanners_points_loop(new_with_points, new_without_points)
      false -> Enum.map(new_with_points, fn {p, _m, _s} -> p end)
    end
  end

  defp find_points([], _without_points, _original), do: []
  defp find_points([_head | tail], [], original), do: find_points(tail, original, original)
  defp find_points([{_point, methods, scanner} | _w_tail] = with_points, [wo_head | wo_tail], original) do
    duplicates = find_duplicates(scanner, wo_head)

    case length(duplicates) < 12 do
      true -> find_points(with_points, wo_tail, original)
      false ->
        {new_point, new_methods} = calculate_point(methods, duplicates)
        rv_head = {new_point, new_methods, wo_head}
        new_original = Enum.reject(original, fn x -> x == wo_head end)
        [rv_head | find_points(with_points, wo_tail, new_original)]
    end
  end

  defp calculate_point(methods, duplicates) do
    {{{d1_x1, d1_y1, d1_z1}, _distances1}, {d1_p2, _distances2}} =
      Enum.at(duplicates, 0)
    {{{d2_x1, d2_y1, d2_z1}, _distances1}, {d2_p2, _distances2}} =
      Enum.at(duplicates, 1)

    {x_loc, _x_flip, _x_add} = x_method =
      find_method(d1_x1, d1_p2, d2_x1, d2_p2, true, true, true)
    {y_loc, _y_flip, _y_add} = y_method =
      find_method(d1_y1, d1_p2, d2_y1, d2_p2, x_loc != :x, x_loc != :y, x_loc != :z)
    z_method =
      find_method(d1_z1, d1_p2, d2_z1, d2_p2, x_loc != :x and y_loc != :x, x_loc != :y and y_loc != :y, x_loc != :z and y_loc != :z)

    new_methods = [{x_method, y_method, z_method} | methods]
    new_point = apply_methods(new_methods, {0, 0, 0})

    {new_point, new_methods}
  end

  defp apply_methods([], point), do: point
  defp apply_methods([{{x_loc, x_flip, x_add}, {y_loc, y_flip, y_add}, {z_loc, z_flip, z_add}} | tail], {x, y, z}) do
    new_x = case x_loc do
      :x -> calculate(x, x_flip, x_add)
      :y -> calculate(y, x_flip, x_add)
      :z -> calculate(z, x_flip, x_add)
    end

    new_y = case y_loc do
      :x -> calculate(x, y_flip, y_add)
      :y -> calculate(y, y_flip, y_add)
      :z -> calculate(z, y_flip, y_add)
    end

    new_z = case z_loc do
      :x -> calculate(x, z_flip, z_add)
      :y -> calculate(y, z_flip, z_add)
      :z -> calculate(z, z_flip, z_add)
    end

    apply_methods(tail, {new_x, new_y, new_z})
  end

  defp calculate(start, flip, add) do
    if flip do
      add - start
    else
      start + add
    end
  end

  defp find_method(d1_v1, {d1_x2, d1_y2, d1_z2}, d2_v1, {d2_x2, d2_y2, d2_z2}, x_ok, y_ok, z_ok) do
    cond do
      x_ok and d1_x2 - d1_v1 == d2_x2 - d2_v1 -> {:x, false, d1_v1 - d1_x2}
      x_ok and d1_x2 + d1_v1 == d2_x2 + d2_v1 -> {:x, true, d1_x2 + d1_v1}
      y_ok and d1_y2 - d1_v1 == d2_y2 - d2_v1 -> {:y, false, d1_v1 - d1_y2}
      y_ok and d1_y2 + d1_v1 == d2_y2 + d2_v1 -> {:y, true, d1_y2 + d1_v1}
      z_ok and d1_z2 - d1_v1 == d2_z2 - d2_v1 -> {:z, false, d1_v1 - d1_z2}
      z_ok and d1_z2 + d1_v1 == d2_z2 + d2_v1 -> {:z, true, d1_z2 + d1_v1}
    end
  end

  defp manhattan_distances(points) do
    len = length(points)

    manhattan_distances_loop(points, 0, 1, len)
  end

  defp manhattan_distances_loop(_points, len, _two, len), do: []
  defp manhattan_distances_loop(points, one, len, len) do
    manhattan_distances_loop(points, one + 1, one + 2, len)
  end
  defp manhattan_distances_loop(points, one, two, len) do
    point1 = Enum.at(points, one)
    point2 = Enum.at(points, two)

    [manhattan_distance(point1, point2) | manhattan_distances_loop(points, one, two + 1, len)]
  end

  defp manhattan_distance({x1, y1, z1}, {x2, y2, z2}) do
    xd = positive_difference(x1, x2)
    yd = positive_difference(y1, y2)
    zd = positive_difference(z1, z2)

    xd + yd + zd
  end

  defp positive_difference(one, two) when one >= two, do: one - two
  defp positive_difference(one, two), do: two - one
end
