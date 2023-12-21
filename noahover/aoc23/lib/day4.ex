defmodule Day4 do
  @moduledoc """
  Advent of Code 2023 Day 4
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{}, fn x, acc ->
      colon_split = String.split(x, ": ")

      id =
        colon_split
        |> hd()
        |> String.trim_leading("Card ")
        |> String.trim(" ")
        |> String.to_integer()

      bar_split =
        colon_split
        |> List.last()
        |> String.split(" | ")

      winning_numbers =
        bar_split
        |> hd()
        |> String.split(" ", trim: true)
        |> Enum.map(fn y -> String.to_integer(y) end)

      held_numbers =
        bar_split
        |> List.last()
        |> String.split(" ", trim: true)
        |> Enum.map(fn y -> String.to_integer(y) end)

      card = %{winning_numbers: winning_numbers, held_numbers: held_numbers, instance_count: 1}

      Map.put(acc, id, card)
    end)
  end

  @doc """
  Day 4 Part 1

  Sum the points of all the cards. You get points by
  having a number that is one of the winning numbers.
  The first matching number is worth 1 point and then
  each match after that doubles the score.
  """
  def sum_points(cards) do
    cards
    |> Map.values()
    |> Enum.map(fn x -> to_points(x) end)
    |> Enum.sum()
  end

  defp to_points(%{winning_numbers: winning, held_numbers: held}) do
    held
    |> count_matches(winning)
    |> calculate_points()
  end

  defp count_matches(held, winning) do
    held
    |> Enum.filter(fn x -> Enum.member?(winning, x) end)
    |> length()
  end

  defp calculate_points(0), do: 0

  defp calculate_points(i) do
    :math.pow(2, i - 1)
    |> round()
  end

  @doc """
  Day 4 Part 2

  Count the instances of all of your cards. For each match you
  get on a card or a copy of a card, you gain a copy of the card
  that is N ids more, where N is the number match it is.
  """
  def count_card_instances(cards) do
    len =
      cards
      |> Map.keys()
      |> length()

    cards
    |> update_card_instance_counts(1, len)
    |> Map.values()
    |> Enum.map(fn %{instance_count: ic} -> ic end)
    |> Enum.sum()
  end

  defp update_card_instance_counts(cards, ix, len) when ix > len, do: cards

  defp update_card_instance_counts(cards, ix, len) do
    %{winning_numbers: winning, held_numbers: held, instance_count: count} = Map.get(cards, ix)

    matches = count_matches(held, winning)
    updated_cards = update_necessary_counts(cards, ix + 1, 0, matches, len, count)

    update_card_instance_counts(updated_cards, ix + 1, len)
  end

  defp update_necessary_counts(cards, ix, _match, _matches, len, _count) when ix > len, do: cards
  defp update_necessary_counts(cards, _ix, match, match, _len, _count), do: cards

  defp update_necessary_counts(cards, ix, match, matches, len, count) do
    %{instance_count: old_count} = old_card = Map.get(cards, ix)

    new_count = old_count + count

    new_card = Map.put(old_card, :instance_count, new_count)
    updated_cards = Map.put(cards, ix, new_card)

    update_necessary_counts(updated_cards, ix + 1, match + 1, matches, len, count)
  end
end
