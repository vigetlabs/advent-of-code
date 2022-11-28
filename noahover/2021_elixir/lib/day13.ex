defmodule Day13 do
  @moduledoc """
  Advent of Code 2021 Day 13
  """

  def parse_input(input) do
    {dots_str, folds_str} =
      input
      |> String.split("\n\n", trim: true)
      |> List.to_tuple()

    dots =
      dots_str
      |> String.split("\n", trim: true)
      |> Enum.map(fn x ->
        x
        |> String.split(",", trim: true)
        |> Enum.map(fn y -> String.to_integer(y) end)
        |> List.to_tuple()
      end)

    folds =
      folds_str
      |> String.split("\n", trim: true)
      |> Enum.map(fn x ->
        {axis, num_str} =
          x
          |> String.trim_leading("fold along ")
          |> String.split("=", trim: true)
          |> List.to_tuple()

        num = String.to_integer(num_str)

        {axis, num}
      end)

    {dots, folds}
  end

  @doc """
  Day 13 Part 1

  Count dots after first fold
  """
  def count_first_fold_dots({dots, [{axis, num} | _tail]}) do
    dots
    |> Enum.map(fn x ->
      case axis do
        "x" -> fold_horizontal(x, num)
        "y" -> fold_vertical(x, num)
      end
    end)
    |> MapSet.new()
    |> MapSet.size()
  end

  defp fold_horizontal({x, y}, num) when x < num, do: {x, y}
  defp fold_horizontal({x, y}, num), do: {(2 * num) - x, y}

  defp fold_vertical({x, y}, num) when y < num, do: {x, y}
  defp fold_vertical({x, y}, num), do: {x, (2 * num) - y}

  @doc """
  Day 13 Part 2

  Get code from all folds
  """
  def folded_code({dots, []}), do: output(dots)
  def folded_code({dots, [{axis, num} | tail]}) do
    new_dots =
      dots
      |> Enum.map(fn x ->
        case axis do
          "x" -> fold_horizontal(x, num)
          "y" -> fold_vertical(x, num)
        end
      end)
      |> Enum.uniq()

    folded_code({new_dots, tail})
  end

  defp output(dots) do
    output_loop(dots, 0, 0)
  end

  defp output_loop(_dots, 6, _col), do: ""
  defp output_loop(dots, row, 40), do: "\n" <> output_loop(dots, row + 1, 0)
  defp output_loop(dots, row, col) do
    char = cond do
      Enum.member?(dots, {col, row}) -> "#"
      true -> "."
    end
    char <> output_loop(dots, row, col + 1)
  end
end
