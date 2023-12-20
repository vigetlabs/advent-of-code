defmodule Day2 do
  @moduledoc """
  Advent of Code 2023 Day 2
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      parts = String.split(x, ": ")

      id =
        parts
        |> hd()
        |> String.trim_leading("Game ")
        |> String.to_integer()

      rounds =
        parts
        |> List.last()
        |> String.split("; ")
        |> Enum.map(fn y ->
          y
          |> String.split(", ")
          |> Enum.reduce(%{}, fn z, acc ->
            round_parts = String.split(z, " ")

            count =
              round_parts
              |> hd()
              |> String.to_integer()

            color =
              round_parts
              |> List.last()
              |> String.to_atom()

            Map.put(acc, color, count)
          end)
        end)

      %{id: id, rounds: rounds}
    end)
  end

  @doc """
  Day 2 Part 1

  Sum the IDs of games where it is possible that the game
  bag only contains 12 red cubes, 13 green cubes, and 14
  blue cubes.
  """
  def sum_possible_game_ids(games) do
    games
    |> Enum.map(fn x -> id_if_possible(x) end)
    |> Enum.sum()
  end

  defp id_if_possible(%{id: id, rounds: rounds}) do
    id_if_possible_loop(id, rounds)
  end

  defp id_if_possible_loop(id, []), do: id
  defp id_if_possible_loop(_id, [%{red: red} | _tail]) when red > 12, do: 0
  defp id_if_possible_loop(_id, [%{green: green} | _tail]) when green > 13, do: 0
  defp id_if_possible_loop(_id, [%{blue: blue} | _tail]) when blue > 14, do: 0

  defp id_if_possible_loop(id, [_head | tail]) do
    id_if_possible_loop(id, tail)
  end

  @doc """
  Day 2 Part 1

  Sum the power of the minimum blue, green, and red
  needed to make the game possible
  """
  def sum_game_powers(games) do
    games
    |> Enum.map(fn x -> game_power(x) end)
    |> Enum.sum()
  end

  def game_power(%{rounds: rounds}) do
    rounds
    |> Enum.reduce(%{blue: 0, green: 0, red: 0}, fn round, %{blue: mb, green: mg, red: mr} ->
      rb = Map.get(round, :blue, 0)
      rg = Map.get(round, :green, 0)
      rr = Map.get(round, :red, 0)

      nb =
        if rb > mb do
          rb
        else
          mb
        end

      ng =
        if rg > mg do
          rg
        else
          mg
        end

      nr =
        if rr > mr do
          rr
        else
          mr
        end

      %{blue: nb, green: ng, red: nr}
    end)
    |> Map.values()
    |> Enum.product()
  end
end
