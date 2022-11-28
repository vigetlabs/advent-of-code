defmodule Day14 do
  @moduledoc """
  Advent of Code 2021 Day 14
  """

  def parse_input(input) do
    {template, rules_str} =
      input
      |> String.split("\n\n", trim: true)
      |> List.to_tuple()

    rules =
      rules_str
      |> String.split("\n", trim: true)
      |> Enum.reduce(%{}, fn x, acc ->
        {pair, inserted} =
          x
          |> String.split(" -> ", trim: true)
          |> List.to_tuple()
        Map.put(acc, pair, inserted)
      end)

    {template, rules}
  end

  @doc """
  Day 14 Part 1

  Find difference between most common element and least common element after 10 steps.
  """
  def difference_after_10_steps({template, rules}) do
    difference(template, rules, 10)
  end

  defp difference(template, rules, steps) do
    last_char = String.last(template)

    frequencies =
      template
      |> String.graphemes()
      |> to_pairs_map(%{})
      |> simulate(rules, 0, steps)
      |> to_frequencies_map(last_char)
      |> Map.values()
      |> Enum.sort()

    List.last(frequencies) - List.first(frequencies)
  end

  defp to_pairs_map([], map), do: map
  defp to_pairs_map([_char], map), do: map
  defp to_pairs_map([first | [second | _t] = tail], map) do
    key = first <> second
    old_value = Map.get(map, key, 0)
    new_map = Map.put(map, key, old_value + 1)
    to_pairs_map(tail, new_map)
  end

  defp simulate(template, _rules, max, max), do: template
  defp simulate(template, rules, step, max) do
    template
    |> Map.to_list()
    |> Enum.reduce(%{}, fn {k, v}, acc ->
      inserted = Map.get(rules, k)
      [first, second] = String.graphemes(k)
      first_pair = first <> inserted
      old_first = Map.get(acc, first_pair, 0)
      new_acc = Map.put(acc, first_pair, old_first + v)
      second_pair = inserted <> second
      old_second = Map.get(new_acc, second_pair, 0)
      Map.put(new_acc, second_pair, old_second + v)
    end)
    |> simulate(rules, step + 1, max)
  end

  defp to_frequencies_map(map, last_char) do
    frequencies_map =
      map
      |> Map.to_list()
      |> Enum.reduce(%{}, fn {k, v}, acc ->
        [first, _second] = String.graphemes(k)
        old_value = Map.get(acc, first, 0)
        Map.put(acc, first, old_value + v)
      end)

    old_value = Map.get(frequencies_map, last_char, 0)
    Map.put(frequencies_map, last_char, old_value + 1)
  end

  @doc """
  Day 14 Part 2

  Find difference between most common element and least common element after 40 steps.
  """
  def difference_after_40_steps({template, rules}) do
    difference(template, rules, 40)
  end
end
