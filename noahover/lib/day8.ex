defmodule Day8 do
  @moduledoc """
  Advent of Code 2021 Day 8
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      x
      |> String.split(" | ", trim: true)
      |> Enum.map(fn y ->
        y
        |> String.split(" ", trim: true)
        |> Enum.map(fn z ->
          z
          |> String.graphemes()
          |> Enum.sort()
          |> Enum.join()
        end)
      end)
      |> List.to_tuple()
    end)
  end

  @doc """
  Day 8 Part 1

  Find the number of unique lengths of the output strings
  """
  def unique_lengths(rows) do
    rows
    |> count_unique()
  end

  defp count_unique([]), do: 0
  defp count_unique([{_inputs, outputs} | tail]) do
    count = Enum.count(outputs, fn x ->
      len = String.length(x)
      len == 2 or len == 3 or len == 4 or len == 7
    end)
    count + count_unique(tail)
  end

  @doc """
  Day 8 Part 2

  Sum the decoded numbers
  """
  def sum_decoded(rows) do
    rows
    |> Enum.map(fn x -> decode(x) end)
    |> Enum.sum()
  end

  defp decode({inputs, outputs}) do
    map = map_inputs(inputs)

    outputs
    |> Enum.map(fn x -> Map.get(map, x) end)
    |> Enum.join()
    |> String.to_integer()
  end

  defp map_inputs(inputs) do
    one = Enum.find(inputs, fn x -> String.length(x) == 2 end)
    inputs = List.delete(inputs, one)
    four = Enum.find(inputs, fn x -> String.length(x) == 4 end)
    inputs = List.delete(inputs, four)
    seven = Enum.find(inputs, fn x -> String.length(x) == 3 end)
    inputs = List.delete(inputs, seven)
    eight = Enum.find(inputs, fn x -> String.length(x) == 7 end)
    inputs = List.delete(inputs, eight)
    nine = Enum.find(inputs, fn x -> String.length(x) == 6 and sublist?(x, four) end)
    inputs = List.delete(inputs, nine)
    zero = Enum.find(inputs, fn x -> String.length(x) == 6 and sublist?(x, one) end)
    inputs = List.delete(inputs, zero)
    six = Enum.find(inputs, fn x -> String.length(x) == 6 end)
    inputs = List.delete(inputs, six)
    three = Enum.find(inputs, fn x -> sublist?(x, one) end)
    inputs = List.delete(inputs, three)
    five = Enum.find(inputs, fn x -> sublist?(six, x) end)
    inputs = List.delete(inputs, five)
    two = Enum.at(inputs, 0)

    %{}
    |> Map.put(zero, "0")
    |> Map.put(one, "1")
    |> Map.put(two, "2")
    |> Map.put(three, "3")
    |> Map.put(four, "4")
    |> Map.put(five, "5")
    |> Map.put(six, "6")
    |> Map.put(seven, "7")
    |> Map.put(eight, "8")
    |> Map.put(nine, "9")
  end

  defp sublist?(str1, str2) do
    sublist_loop(String.graphemes(str1), String.graphemes(str2))
  end

  defp sublist_loop(_list1, []), do: true
  defp sublist_loop(list1, [head | tail]) do
    Enum.member?(list1, head) and sublist_loop(list1, tail)
  end
end
