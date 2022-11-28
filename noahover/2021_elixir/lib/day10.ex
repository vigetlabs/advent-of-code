defmodule Day10 do
  @moduledoc """
  Advent of Code 2021 Day 10
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
  end

  @doc """
  Day 10 Part 1

  Syntax error score
  """
  def syntax_error_score(lines) do
    lines
    |> Enum.map(fn x -> score_line(x) end)
    |> Enum.sum()
  end

  defp score_line(line) do
    line
    |> String.graphemes()
    |> mistake_character([])
    |> character_score()
  end

  defp mistake_character([], chars), do: {:incomplete, chars}
  defp mistake_character([head | tail], chars) do
    case head do
      "(" -> mistake_character(tail, [")" | chars])
      "[" -> mistake_character(tail, ["]" | chars])
      "{" -> mistake_character(tail, ["}" | chars])
      "<" -> mistake_character(tail, [">" | chars])
      _ ->
        if length(chars) > 0 do
          [chars_head | chars_tail] = chars
          if chars_head == head do
            mistake_character(tail, chars_tail)
          else
            {:corrupted, head}
          end
        else
          {:corrupted, head}
        end
    end
  end

  defp character_score({:corrupted, char}) do
    case char do
      ")" -> 3
      "]" -> 57
      "}" -> 1197
      ">" -> 25137
      _ -> 0
    end
  end
  defp character_score(_), do: 0

  @doc """
  Day 10 Part 2

  Incomplete syntax error score
  """
  def incomplete_score(lines) do
    scores =
      lines
      |> Enum.map(fn x -> score_incomplete(x) end)
      |> Enum.filter(fn x -> x > 0 end)
      |> Enum.sort()

    Enum.at(scores, div(length(scores), 2))
  end

  defp score_incomplete(line) do
    line
    |> String.graphemes()
    |> mistake_character([])
    |> characters_score()
  end

  defp characters_score({:incomplete, chars}) do
    Enum.reduce(chars, 0, fn x, acc -> (5 * acc) + char_score(x) end)
  end
  defp characters_score(_), do: 0

  defp char_score(char) do
    case char do
      ")" -> 1
      "]" -> 2
      "}" -> 3
      ">" -> 4
    end
  end
end
