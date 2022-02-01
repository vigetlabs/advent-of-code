defmodule Day21 do
  @moduledoc """
  Advent of Code 2021 Day 21
  """

  def parse_input(input) do
    {player1_str, player2_str} =
      input
      |> String.split("\n", trim: true)
      |> List.to_tuple()

    player1 = convert_to_player(player1_str)
    player2 = convert_to_player(player2_str)

    {player1, player2}
  end

  defp convert_to_player(str) do
    position =
      str
      |> String.split(": ", trim: true)
      |> Enum.at(1)
      |> String.to_integer()

    %{position: position, score: 0}
  end

  @doc """
  Day 21 Part 1

  Get product of loser's score and number of dice rolls
  """
  def score_game({player1, player2}) do
    play_game(player1, player2, 0, 1)
  end

  defp play_game(%{score: score1}, %{score: score2}, rolls, _turn) when score1 >= 1000 do
    score2 * rolls
  end
  defp play_game(%{score: score1}, %{score: score2}, rolls, _turn) when score2 >= 1000 do
    score1 * rolls
  end
  defp play_game(player1, player2, rolls, 1) do
    player1
    |> take_turn(rolls)
    |> play_game(player2, rolls + 3, 2)
  end
  defp play_game(player1, player2, rolls, 2) do
    new_player2 = take_turn(player2, rolls)
    play_game(player1, new_player2, rolls + 3, 1)
  end

  defp take_turn(%{position: position, score: score}, rolls) do
    total_roll = roll(rolls + 1) + roll(rolls + 2) + roll(rolls + 3)

    new_position = update_position(position, total_roll)
    new_score = score + new_position

    %{position: new_position, score: new_score}
  end

  defp roll(num) do
    case rem(num, 100) do
      0 -> 100
      r -> r
    end
  end

  defp update_position(position, total_roll) do
    case rem(position + total_roll, 10) do
      0 -> 10
      r -> r
    end
  end

  @doc """
  Day 21 Part 2

  Count universes where more common winner wins
  """
  def count_universes({%{position: pos1, score: score1}, %{position: pos2, score: score2}}) do
    %{{pos1, score1, pos2, score2} => 1}
    |> play_real_game(0)
    |> Map.to_list()
    |> Enum.reduce(%{one: 0, two: 0}, fn {{_p1, s1, _p2, s2}, v}, acc ->
      cond do
        s1 == :win ->
          old = Map.get(acc, :one)
          Map.put(acc, :one, old + v)

        s2 == :win ->
          old = Map.get(acc, :two)
          Map.put(acc, :two, old + v)
      end
    end)
    |> Map.values()
    |> Enum.max()
  end

  defp play_real_game(state_map, 42), do: state_map
  defp play_real_game(state_map, round) do
    turn = rem(round, 2) + 1

    rv = state_map
    |> Map.to_list()
    |> take_real_turn(turn, %{})
    |> play_real_game(round + 1)
  end

  defp take_real_turn([], _turn, map), do: map
  defp take_real_turn([{{_p1, :win, _p2, _s2} = k, v} | tail], turn, map) do
    old_value = Map.get(map, k, 0)
    take_real_turn(tail, turn, Map.put(map, k, old_value + v))
  end
  defp take_real_turn([{{_p1, _s1, _p2, :win} = k, v} | tail], turn, map) do
    old_value = Map.get(map, k, 0)
    take_real_turn(tail, turn, Map.put(map, k, old_value + v))
  end
  defp take_real_turn([head | tail], turn, map) do
    new_map =
      map
      |> put_state(head, turn, 3, 1)
      |> put_state(head, turn, 4, 3)
      |> put_state(head, turn, 5, 6)
      |> put_state(head, turn, 6, 7)
      |> put_state(head, turn, 7, 6)
      |> put_state(head, turn, 8, 3)
      |> put_state(head, turn, 9, 1)

    take_real_turn(tail, turn, new_map)
  end

  defp put_state(map, {{p1, s1, p2, s2}, v}, 1, add, mult) do
    new_pos = update_position(p1, add)
    new_score = case new_pos + s1 do
      s when s >= 21 -> :win
      s -> s
    end

    key = {new_pos, new_score, p2, s2}

    old_value = Map.get(map, key, 0)
    new_value = old_value + (v * mult)

    Map.put(map, key, new_value)
  end
  defp put_state(map, {{p1, s1, p2, s2}, v}, 2, add, mult) do
    new_pos = update_position(p2, add)
    new_score = case new_pos + s2 do
      s when s >= 21 -> :win
      s -> s
    end

    key = {p1, s1, new_pos, new_score}

    old_value = Map.get(map, key, 0)
    new_value = old_value + (v * mult)

    Map.put(map, key, new_value)
  end
end
