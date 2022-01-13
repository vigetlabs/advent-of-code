defmodule Day20 do
  @moduledoc """
  Advent of Code 2021 Day 20
  """

  def parse_input(input) do
    {algorithm, input_image_str} =
      input
      |> String.split("\n\n", trim: true)
      |> List.to_tuple()

    input =
      input_image_str
      |> String.split("\n", trim: true)
      |> Enum.map(fn x ->
        x
        |> String.graphemes()
        |> to_map(0, %{})
      end)
      |> to_map(0, %{})

    {algorithm, input}
  end

  defp to_map([], _index, map), do: map
  defp to_map([head | tail], index, map) do
    to_map(tail, index + 1, Map.put(map, index, head))
  end

  @doc """
  Day 20 Part 1

  Count lit pixels after two enhancements
  """
  def count_lit({algorithm, input}) do
    input
    |> enhance(algorithm, 1)
    |> enhance(algorithm, 2)
    |> Map.values()
    |> Enum.map(fn x -> Map.values(x) end)
    |> List.flatten()
    |> Enum.count(fn x -> x == "#" end)
  end

  defp enhance(input, algorithm, num) do
    len =
      input
      |> Map.keys()
      |> length()

    default = case String.at(algorithm, 0) == "#" and rem(num, 2) == 0 do
      true -> "1"
      false -> "0"
    end

    new_len = len + 2
    row = create_map(%{}, 0, new_len, ".")
    output = create_map(%{}, 0, new_len, row)

    enhance_loop(output, 0, 0, new_len, input, algorithm, len, default)
  end

  defp create_map(map, len, len, _val), do: map
  defp create_map(map, ix, len, val) do
    map
    |> Map.put(ix, val)
    |> create_map(ix + 1, len, val)
  end

  defp enhance_loop(output, len, _col, len, _input, _algorithm, _old_len, _default), do: output
  defp enhance_loop(output, row, len, len, input, algorithm, old_len, default) do
    enhance_loop(output, row + 1, 0, len, input, algorithm, old_len, default)
  end
  defp enhance_loop(output, row, col, len, input, algorithm, old_len, default) do
    value = value_at(input, algorithm, row, col, old_len, default)

    output
    |> put_in([row, col], value)
    |> enhance_loop(row, col + 1, len, input, algorithm, old_len, default)
  end

  defp value_at(input, algorithm, row, col, len, default) do
    ix =
      [
        bit_at(input, row - 2, col - 2, len, default),
        bit_at(input, row - 2, col - 1, len, default),
        bit_at(input, row - 2, col, len, default),
        bit_at(input, row - 1, col - 2, len, default),
        bit_at(input, row - 1, col - 1, len, default),
        bit_at(input, row - 1, col, len, default),
        bit_at(input, row, col - 2, len, default),
        bit_at(input, row, col - 1, len, default),
        bit_at(input, row, col, len, default)
      ]
      |> Enum.join()
      |> String.to_integer(2)

    String.at(algorithm, ix)
  end

  defp bit_at(_input, row, col, len, default) when row < 0 or col < 0 or row >= len or col >= len do
    default
  end
  defp bit_at(input, row, col, _len, _default) do
    case input[row][col] do
      "." -> "0"
      "#" -> "1"
    end
  end

  @doc """
  Day 20 Part 2

  Count lit pixels after fifty enhancements
  """
  def count_lit_after_fifty({algorithm, input}) do
    input
    |> enhance_number(algorithm, 1, 50)
    |> Map.values()
    |> Enum.map(fn x -> Map.values(x) end)
    |> List.flatten()
    |> Enum.count(fn x -> x == "#" end)
  end

  defp enhance_number(input, _algorithm, num, max) when num > max, do: input
  defp enhance_number(input, algorithm, num, max) do
    input
    |> enhance(algorithm, num)
    |> enhance_number(algorithm, num + 1, max)
  end
end
