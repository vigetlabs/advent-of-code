defmodule Day4 do
  @moduledoc """
  Advent of Code 2021 Day 4
  """

  def parse_input(input) do
    [numbers_str | cards_arr] =
      input
      |> String.split("\n\n", trim: true)

    numbers =
      numbers_str
      |> String.split(",", trim: true)
      |> Enum.map(fn x -> String.to_integer(x) end)

    cards = parse_cards(cards_arr)

    {numbers, cards}
  end

  defp parse_cards([]), do: []
  defp parse_cards([head | tail]) do
    card =
      head
      |> String.split("\n", trim: true)
      |> Enum.map(fn x ->
        x
        |> String.split(" ", trim: true)
        |> Enum.map(fn y -> String.to_integer(y) end)
        |> to_map(0, %{})
      end)
      |> to_map(0, %{})
    [card | parse_cards(tail)]
  end

  defp to_map([], _index, map), do: map
  defp to_map([head | tail], index, map) do
    to_map(tail, index + 1, Map.put(map, index, head))
  end

  @doc """
  Day 4 Part 1

  Find bingo winner
  """
  def bingo_winner([], _cards), do: -1
  def bingo_winner([head | tail], cards) do
    case mark_cards(cards, head, []) do
      {:winner, value} -> value
      {:not_yet, new_cards} -> bingo_winner(tail, new_cards)
    end
  end

  defp mark_cards([], _called, new_cards), do: {:not_yet, new_cards}
  defp mark_cards([head | tail], called, new_cards) do
    case mark_card(head, called, 0, 0) do
      {:winner, winner} -> {:winner, calculate_value(winner, called)}
      card -> mark_cards(tail, called, [card | new_cards])
    end
  end

  defp mark_card(card, _called, 5, _col), do: card
  defp mark_card(card, called, row, 5), do: mark_card(card, called, row + 1, 0)
  defp mark_card(card, called, row, col) do
    if card[row][col] == called do
      new_card = put_in(card, [row, col], :marked)
      cond do
        check_row(new_card, row) -> {:winner, new_card}
        check_col(new_card, col) -> {:winner, new_card}
        true -> new_card
      end
    else
      mark_card(card, called, row, col + 1)
    end
  end

  defp check_row(card, row) do
    card[row][0] == :marked and card[row][1] == :marked and card[row][2] == :marked and card[row][3] == :marked and card[row][4] == :marked
  end

  defp check_col(card, col) do
    card[0][col] == :marked and card[1][col] == :marked and card[2][col] == :marked and card[3][col] == :marked and card[4][col] == :marked
  end

  defp calculate_value(card, multiplier) do
    sum =
      card
      |> Map.to_list()
      |> Enum.map(fn {_k, v} ->
        v
        |> Map.to_list()
        |> Enum.map(fn {_k2, v2} ->
          case v2 do
            :marked -> 0
            val -> val
          end
        end)
        |> Enum.sum()
      end)
      |> Enum.sum()
    sum * multiplier
  end

  @doc """
  Day 4 Part 2

  Find the last bingo card to win
  """
  def final_bingo_winner([], _cards), do: -1
  def final_bingo_winner([head | tail], cards) do
    case mark_and_remove_cards(cards, head, []) do
      {:final_winner, value} -> value
      {:not_yet, new_cards} -> final_bingo_winner(tail, new_cards)
    end
  end

  defp mark_and_remove_cards([], _called, new_cards), do: {:not_yet, new_cards}
  defp mark_and_remove_cards([head | tail], called, new_cards) do
    case mark_card(head, called, 0, 0) do
      {:winner, winner} ->
        if length(new_cards) + length(tail) > 0 do
          mark_and_remove_cards(tail, called, new_cards)
        else
          {:final_winner, calculate_value(winner, called)}
        end

      card -> mark_and_remove_cards(tail, called, [card | new_cards])
    end
  end
end
