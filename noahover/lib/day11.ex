defmodule Day11 do
  @moduledoc """
  Advent of Code 2021 Day 11
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      x
      |> String.graphemes()
      |> Enum.map(fn y -> String.to_integer(y) end)
    end)
  end

  @doc """
  Day 11 Part 1

  Count octopus flashes
  """
  def octopus_flashes(grid) do
    simulate_days(grid, 0, 100)
  end

  defp simulate_days(_grid, last, last), do: 0
  defp simulate_days(grid, day, last) do
    {new_grid, flashes} = simulate_day(grid)
    flashes + simulate_days(new_grid, day + 1, last)
  end

  defp simulate_day(grid) do
    grid
    |> increment_grid()
    |> additional_flashes(0)
    |> repair_grid()
  end

  defp increment_grid(grid) do
    Enum.map(grid, fn x ->
      Enum.map(x, fn y ->
        case y do
          9 -> {:flash, 0}
          _ -> y + 1
        end
      end)
    end)
  end

  defp additional_flashes(grid, rd) do
    num_rows = length(grid)
    num_cols = length(Enum.at(grid, 0))

    extra_energy = additional_flash_loop(grid, rd, 0, 0, num_rows, num_cols)

    case extra_energy do
      [] -> grid
      _ ->
        new_grid = add_energy(grid, rd + 1, extra_energy)
        additional_flashes(new_grid, rd + 1)
    end
  end

  defp additional_flash_loop(_grid, _rd, num_rows, _col, num_rows, _num_cols), do: []
  defp additional_flash_loop(grid, rd, row, num_cols, num_rows, num_cols) do
    additional_flash_loop(grid, rd, row + 1, 0, num_rows, num_cols)
  end
  defp additional_flash_loop(grid, rd, row, col, num_rows, num_cols) do
    curr_row = Enum.at(grid, row, [])
    value = Enum.at(curr_row, col)

    case value do
      {:flash, _rd} -> additional_flash_loop(grid, rd, row, col + 1, num_rows, num_cols)
      _ ->
        count = check_neighbors(grid, rd, row, col)

        if count > 0 do
          [{row, col, count} | additional_flash_loop(grid, rd, row, col + 1, num_rows, num_cols)]
        else
          additional_flash_loop(grid, rd, row, col + 1, num_rows, num_cols)
        end
    end
  end

  defp check_neighbors(grid, rd, row, col) do
    upleft = check(grid, rd, row - 1, col - 1)
    up = check(grid, rd, row - 1, col)
    upright = check(grid, rd, row - 1, col + 1)
    right = check(grid, rd, row, col + 1)
    downright = check(grid, rd, row + 1, col + 1)
    down = check(grid, rd, row + 1, col)
    downleft = check(grid, rd, row + 1, col - 1)
    left = check(grid, rd, row, col - 1)

    upleft + up + upright + right + downright + down + downleft + left
  end

  defp check(_grid, _rd, row, col) when row < 0 or col < 0, do: 0
  defp check(grid, rd, row, col) do
    curr_row = Enum.at(grid, row, [])
    value = Enum.at(curr_row, col)

    case value do
      {:flash, round} when round == rd -> 1
      _ -> 0
    end
  end

  defp add_energy(grid, _rd, []), do: grid
  defp add_energy(grid, rd, [{row, col, count} | tail]) do
    curr_row = Enum.at(grid, row)
    value = Enum.at(curr_row, col)

    new_value = cond do
      value + count > 9 -> {:flash, rd}
      true -> value + count
    end

    new_row = List.replace_at(curr_row, col, new_value)

    grid
    |> List.replace_at(row, new_row)
    |> add_energy(rd, tail)
  end

  defp repair_grid(grid) do
    flashes = Enum.reduce(grid, 0, fn x, acc ->
      acc + Enum.count(x, fn y ->
        case y do
          {:flash, _rd} -> true
          _ -> false
        end
      end)
    end)

    new_grid = Enum.map(grid, fn x ->
      Enum.map(x, fn y ->
        case y do
          {:flash, _rd} -> 0
          _ -> y
        end
      end)
    end)

    {new_grid, flashes}
  end

  @doc """
  Day 11 Part 2

  Find simultaneous flash
  """
  def simultaneous_flash(grid, day) do
    {new_grid, _flashes} = simulate_day(grid)

    bad_values =
      new_grid
      |> List.flatten()
      |> Enum.filter(fn x -> x > 0 end)

    if length(bad_values) > 0 do
      simultaneous_flash(new_grid, day + 1)
    else
      day
    end
  end
end
