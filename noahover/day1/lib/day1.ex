defmodule Day1 do
  @moduledoc """
  Advent of Code 2021 Day 1
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x -> String.to_integer(x) end)
  end

  @doc """
  Day 1 Part 1

  Find number of times depth measurement increase
  """
  def count_increases([]), do: 0
  def count_increases([_val]), do: 0
  def count_increases([head | [tail_head | _tail_tail] = tail]) do
    cond do
      tail_head > head -> 1 + count_increases(tail)
      true -> count_increases(tail)
    end
  end

  @doc """
  Day 1 Part 2

  Find number of times sum of three adjacent depth measurements increases
  """
  def count_sum_increases(list) when length(list) < 4, do: 0
  def count_sum_increases([first | [second | [third | [fourth | _fourth_tail]]] = tail]) do
    first_sum = first + second + third
    second_sum = second + third + fourth
    cond do
      second_sum > first_sum -> 1 + count_sum_increases(tail)
      true -> count_sum_increases(tail)
    end
  end
end
