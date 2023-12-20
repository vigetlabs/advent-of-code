defmodule Day3 do
  @moduledoc """
  Advent of Code 2023 Day 3
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      x
      |> String.split("", trim: true)
      |> to_map(0, %{})
    end)
    |> to_map(0, %{})
  end

  defp to_map([], _index, map), do: map

  defp to_map([head | tail], index, map) do
    to_map(tail, index + 1, Map.put(map, index, head))
  end

  @doc """
  Day 3 Part 1

  Sums the part numbers
  """
  def sum_part_numbers(grid) do
    grid
    |> to_part_numbers()
    |> Enum.sum()
  end

  defp to_part_numbers(grid) do
    rows =
      grid
      |> Map.keys()
      |> length()

    cols =
      grid
      |> Map.get(0)
      |> Map.keys()
      |> length()

    to_part_numbers_loop(grid, 0, 0, rows, cols, [])
  end

  defp to_part_numbers_loop(_grid, row, _col, row, _cols, nums), do: nums

  defp to_part_numbers_loop(grid, row, cols, rows, cols, nums) do
    to_part_numbers_loop(grid, row + 1, 0, rows, cols, nums)
  end

  defp to_part_numbers_loop(grid, row, col, rows, cols, nums) do
    curr = get_curr(grid, row, col)

    case Integer.parse(curr) do
      {int, _decimal} ->
        new_num = get_part_number(grid, row, col + 1, cols, int)

        num_length =
          new_num
          |> Integer.to_string()
          |> String.length()

        if check_if_part_number(grid, row, col, rows, cols, num_length) do
          to_part_numbers_loop(grid, row, col + num_length, rows, cols, [new_num | nums])
        else
          to_part_numbers_loop(grid, row, col + num_length, rows, cols, nums)
        end

      :error ->
        to_part_numbers_loop(grid, row, col + 1, rows, cols, nums)
    end
  end

  defp get_part_number(_grid, _row, col, col, num), do: num

  defp get_part_number(grid, row, col, cols, num) do
    curr = get_curr(grid, row, col)

    case Integer.parse(curr) do
      {int, _decimal} ->
        get_part_number(grid, row, col + 1, cols, num * 10 + int)

      :error ->
        num
    end
  end

  defp get_curr(grid, row, col) do
    grid
    |> Map.get(row)
    |> Map.get(col)
  end

  defp check_if_part_number(grid, row, col, rows, cols, len) do
    check_for_part(grid, row, col - 1, rows, cols) or
      check_for_part(grid, row - 1, col - 1, rows, cols) or
      check_for_part(grid, row + 1, col - 1, rows, cols) or
      check_for_part(grid, row, col + len, rows, cols) or
      check_for_part(grid, row - 1, col + len, rows, cols) or
      check_for_part(grid, row + 1, col + len, rows, cols) or
      check_for_part_vertically(grid, row, col, col + len, rows, cols)
  end

  defp check_for_part(_grid, -1, _col, _rows, _cols), do: false
  defp check_for_part(_grid, _row, -1, _rows, _cols), do: false
  defp check_for_part(_grid, row, _col, row, _cols), do: false
  defp check_for_part(_grid, _row, col, _rows, col), do: false

  defp check_for_part(grid, row, col, _rows, _cols) do
    curr = get_curr(grid, row, col)

    case Integer.parse(curr) do
      :error ->
        case curr do
          "." ->
            false

          _ ->
            true
        end

      _ ->
        false
    end
  end

  defp check_for_part_vertically(_grid, _row, col, col, _rows, _cols), do: false

  defp check_for_part_vertically(grid, row, col, final, rows, cols) do
    check_for_part(grid, row - 1, col, rows, cols) or
      check_for_part(grid, row + 1, col, rows, cols) or
      check_for_part_vertically(grid, row, col + 1, final, rows, cols)
  end
end
