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
    rows = get_rows_count(grid)
    cols = get_cols_count(grid)

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
        num_length = get_num_length(new_num)

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

  defp get_rows_count(grid) do
    grid
    |> Map.keys()
    |> length()
  end

  defp get_cols_count(grid) do
    grid
    |> Map.get(0)
    |> Map.keys()
    |> length()
  end

  defp get_num_length(num) do
    num
    |> Integer.to_string()
    |> String.length()
  end

  @doc """
  Day 3 Part 2

  Sums the ratios of the gears, which are any * beside
  exactly two part numbers. The ratio is found by multiplying
  those two part numbers.
  """
  def sum_gear_ratios(grid) do
    grid
    |> to_gear_ratios()
    |> Enum.sum()
  end

  defp to_gear_ratios(grid) do
    rows = get_rows_count(grid)
    cols = get_cols_count(grid)

    to_gear_ratios_loop(grid, 0, 0, rows, cols, [], 1)
  end

  defp to_gear_ratios_loop(_grid, row, _col, row, _cols, ratios, _count), do: ratios

  defp to_gear_ratios_loop(grid, row, cols, rows, cols, ratios, count) do
    to_gear_ratios_loop(grid, row + 1, 0, rows, cols, ratios, count)
  end

  defp to_gear_ratios_loop(grid, row, col, rows, cols, ratios, count) do
    case get_curr(grid, row, col) do
      "*" ->
        case get_gear_ratio(grid, row, col, rows, cols) do
          :not_gear ->
            to_gear_ratios_loop(grid, row, col + 1, rows, cols, ratios, count + 1)

          ratio ->
            to_gear_ratios_loop(grid, row, col + 1, rows, cols, [ratio | ratios], count)
        end

      _ ->
        to_gear_ratios_loop(grid, row, col + 1, rows, cols, ratios, count)
    end
  end

  defp get_gear_ratio(grid, row, col, rows, cols) do
    right_part_numbers = get_right_part_numbers(grid, row, col + 1, cols)
    left_part_numbers = get_left_part_numbers(grid, row, col - 1)
    up_part_numbers = get_vertical_part_numbers(grid, row - 1, col, rows, cols)
    down_part_numbers = get_vertical_part_numbers(grid, row + 1, col, rows, cols)

    part_numbers = left_part_numbers ++ right_part_numbers ++ up_part_numbers ++ down_part_numbers

    if length(part_numbers) == 2 do
      Enum.product(part_numbers)
    else
      :not_gear
    end
  end

  defp get_right_part_numbers(_grid, _row, col, col), do: []

  defp get_right_part_numbers(grid, row, col, cols) do
    curr = get_curr(grid, row, col)

    case Integer.parse(curr) do
      {int, _decimal} ->
        part_number = get_part_number(grid, row, col + 1, cols, int)
        [part_number]

      :error ->
        []
    end
  end

  defp get_left_part_numbers(_grid, _row, -1), do: []

  defp get_left_part_numbers(grid, row, col) do
    curr = get_curr(grid, row, col)

    case Integer.parse(curr) do
      {int, _decimal} ->
        part_number = get_reverse_part_number(grid, row, col - 1, int, 1)
        [part_number]

      :error ->
        []
    end
  end

  defp get_reverse_part_number(_grid, _row, -1, num, _len), do: num

  defp get_reverse_part_number(grid, row, col, num, len) do
    curr = get_curr(grid, row, col)

    {multiplier, adder} =
      case num do
        nil ->
          {1, 0}

        _ ->
          m = :math.pow(10, len) |> round()
          {m, num}
      end

    case Integer.parse(curr) do
      {int, _decimal} ->
        get_reverse_part_number(grid, row, col - 1, int * multiplier + adder, len + 1)

      :error ->
        adder
    end
  end

  defp get_vertical_part_numbers(_grid, -1, _col, _rows, _cols), do: []
  defp get_vertical_part_numbers(_grid, row, _col, row, _cols), do: []

  defp get_vertical_part_numbers(grid, row, col, _rows, cols) do
    {left_part_number, left_end} = get_part_number_both_ways(grid, row, col - 1, cols)

    case left_part_number do
      nil ->
        {center_part_number, _center_end} = get_part_number_both_ways(grid, row, col, cols)

        case center_part_number do
          nil ->
            {right_part_number, _right_end} = get_part_number_both_ways(grid, row, col + 1, cols)

            case right_part_number do
              nil ->
                []

              _ ->
                [right_part_number]
            end

          _ ->
            [center_part_number]
        end

      _ ->
        if left_end >= col do
          [left_part_number]
        else
          {right_part_number, _right_end} = get_part_number_both_ways(grid, row, col + 1, cols)

          case right_part_number do
            nil ->
              [left_part_number]

            _ ->
              [left_part_number, right_part_number]
          end
        end
    end
  end

  defp get_part_number_both_ways(_grid, _row, -1, _cols), do: {nil, nil}
  defp get_part_number_both_ways(_grid, _row, col, col), do: {nil, nil}

  defp get_part_number_both_ways(grid, row, col, cols) do
    curr = get_curr(grid, row, col)

    case Integer.parse(curr) do
      {int, _decimal} ->
        reverse_number = get_reverse_part_number(grid, row, col - 1, nil, 0)
        forward_number = get_part_number(grid, row, col + 1, cols, int)
        forward_str = Integer.to_string(forward_number)

        forward_length =
          if int == 0 and String.first(forward_str) != "0" do
            tmp_str = "0" <> forward_str
            String.length(tmp_str)
          else
            String.length(forward_str)
          end

        multiplier = :math.pow(10, forward_length) |> round()
        part_number = reverse_number * multiplier + forward_number
        number_end = col + forward_length - 1

        {part_number, number_end}

      :error ->
        {nil, nil}
    end
  end
end
