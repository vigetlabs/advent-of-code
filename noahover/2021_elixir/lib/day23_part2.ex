defmodule Day23Part2 do
  @moduledoc """
  Advent of Code 2021 Day 23 Part 2
  """

  def parse_input(input) do
    {alist, blist, clist, dlist} =
      input
      |> String.split("\n", trim: true)
      |> Enum.slice(2, 2)
      |> Enum.map(fn x ->
        x
        |> String.trim()
        |> String.split("#", trim: true)
      end)
      |> List.zip()
      |> Enum.map(fn x ->
        x
        |> Tuple.to_list()
      end)
      |> List.to_tuple()

    a = to_map(alist, 0, "a", %{})
    b = to_map(blist, 0, "b", a)
    c = to_map(clist, 0, "c", b)
    d = to_map(dlist, 0, "d", c)

    "."
    |> List.duplicate(11)
    |> to_map(0, "h", d)
    |> Map.put(:energy, 0)
    |> Map.put(:heuristic, 0)
  end

  defp to_map([], _index, _label, map), do: map
  defp to_map(list, 1, label, map) do
    key =
      label <> "1"
      |> String.to_atom()

    value = case key do
      :a1 -> "D"
      :b1 -> "C"
      :c1 -> "B"
      :d1 -> "A"
      _ -> "."
    end

    to_map(list, 2, label, Map.put(map, key, value))
  end
  defp to_map(list, 2, label, map) do
    key =
      label <> "2"
      |> String.to_atom()

    value = case key do
      :a2 -> "D"
      :b2 -> "B"
      :c2 -> "A"
      :d2 -> "C"
      _ -> "."
    end

    to_map(list, 3, label, Map.put(map, key, value))
  end
  defp to_map([head | tail], index, label, map) do
    key =
      label <> Integer.to_string(index)
      |> String.to_atom()

    to_map(tail, index + 1, label, Map.put(map, key, head))
  end

  @doc """
  Find lowest energy it would take to solve amphipod living situation with
  additional amphipods
  """
  def lowest_energy(state) do
    state
    |> set_heuristic()
    |> astar([], [])
  end

  defp set_heuristic(state) do
    a_pos = positions(state, "A")
    b_pos = positions(state, "B")
    c_pos = positions(state, "C")
    d_pos = positions(state, "D")

    heuristic = a_heuristic(a_pos) + b_heuristic(b_pos) + c_heuristic(c_pos) + d_heuristic(d_pos)

    Map.put(state, :heuristic, heuristic)
  end

  defp positions(state, letter) do
    state
    |> Map.to_list()
    |> Enum.filter(fn {_k, v} -> v == letter end)
    |> Enum.map(fn {k, _v} -> k end)
  end

  defp a_heuristic([]), do: 6
  defp a_heuristic([head | tail]) do
    val = case head do
      :a0 -> 4
      :a1 -> 5
      :a2 -> 6
      :a3 -> -3
      :b0 -> 4
      :b1 -> 5
      :b2 -> 6
      :b3 -> 7
      :c0 -> 6
      :c1 -> 7
      :c2 -> 8
      :c3 -> 9
      :d0 -> 8
      :d1 -> 9
      :d2 -> 10
      :d3 -> 11
      :h0 -> 3
      :h1 -> 2
      :h3 -> 2
      :h5 -> 4
      :h7 -> 6
      :h9 -> 8
      :h10 -> 9
    end
    val + a_heuristic(tail)
  end

  defp b_heuristic([]), do: 60
  defp b_heuristic([head | tail]) do
    val = case head do
      :a0 -> 40
      :a1 -> 50
      :a2 -> 60
      :a3 -> 70
      :b0 -> 40
      :b1 -> 50
      :b2 -> 60
      :b3 -> -30
      :c0 -> 40
      :c1 -> 50
      :c2 -> 60
      :c3 -> 70
      :d0 -> 60
      :d1 -> 70
      :d2 -> 80
      :d3 -> 90
      :h0 -> 50
      :h1 -> 40
      :h3 -> 20
      :h5 -> 20
      :h7 -> 40
      :h9 -> 60
      :h10 -> 70
    end
    val + b_heuristic(tail)
  end

  defp c_heuristic([]), do: 600
  defp c_heuristic([head | tail]) do
    val = case head do
      :a0 -> 600
      :a1 -> 700
      :a2 -> 800
      :a3 -> 900
      :b0 -> 400
      :b1 -> 500
      :b2 -> 600
      :b3 -> 700
      :c0 -> 400
      :c1 -> 500
      :c2 -> 600
      :c3 -> -300
      :d0 -> 400
      :d1 -> 500
      :d2 -> 600
      :d3 -> 700
      :h0 -> 700
      :h1 -> 600
      :h3 -> 400
      :h5 -> 200
      :h7 -> 200
      :h9 -> 400
      :h10 -> 500
    end
    val + c_heuristic(tail)
  end

  defp d_heuristic([]), do: 6_000
  defp d_heuristic([head | tail]) do
    val = case head do
      :a0 -> 8_000
      :a1 -> 9_000
      :a2 -> 10_000
      :a3 -> 11_000
      :b0 -> 6_000
      :b1 -> 7_000
      :b2 -> 8_000
      :b3 -> 9_000
      :c0 -> 4_000
      :c1 -> 5_000
      :c2 -> 6_000
      :c3 -> 7_000
      :d0 -> 4_000
      :d1 -> 5_000
      :d2 -> 6_000
      :d3 -> -3_000
      :h0 -> 9_000
      :h1 -> 8_000
      :h3 -> 6_000
      :h5 -> 4_000
      :h7 -> 2_000
      :h9 -> 2_000
      :h10 -> 3_000
    end
    val + d_heuristic(tail)
  end

  defp astar(%{heuristic: 0, energy: energy}, _possible, _old), do: energy
  defp astar(state, possible, old) do
    old_state = Map.drop(state, [:energy, :heuristic])
    new_old = [old_state | old]

    next =
      state
      |> next_steps()
      |> Enum.reject(fn x ->
        x_pos = Map.drop(x, [:energy, :heuristic])
        Enum.member?(new_old, x_pos)
      end)

    new_possible =
      possible ++ next
      |> Enum.sort(fn %{heuristic: xh, energy: xe}, %{heuristic: yh, energy: ye} ->
        xh + xe <= yh + ye
      end)
      |> Enum.uniq_by(fn %{
          a0: a0, a1: a1, a2: a2, a3: a3,
          b0: b0, b1: b1, b2: b2, b3: b3,
          c0: c0, c1: c1, c2: c2, c3: c3,
          d0: d0, d1: d1, d2: d2, d3: d3,
          h0: h0, h1: h1, h3: h3, h5: h5,
          h7: h7, h9: h9, h10: h10
        } ->
        %{
          a0: a0, a1: a1, a2: a2, a3: a3,
          b0: b0, b1: b1, b2: b2, b3: b3,
          c0: c0, c1: c1, c2: c2, c3: c3,
          d0: d0, d1: d1, d2: d2, d3: d3,
          h0: h0, h1: h1, h3: h3, h5: h5,
          h7: h7, h9: h9, h10: h10
        }
      end)

    [head | tail] = new_possible

    astar(head, tail, new_old)
  end

  defp next_steps(state) do
    [
      a0_moves(state),
      a1_moves(state),
      a2_moves(state),
      a3_moves(state),
      b0_moves(state),
      b1_moves(state),
      b2_moves(state),
      b3_moves(state),
      c0_moves(state),
      c1_moves(state),
      c2_moves(state),
      c3_moves(state),
      d0_moves(state),
      d1_moves(state),
      d2_moves(state),
      d3_moves(state),
      h0_moves(state),
      h1_moves(state),
      h3_moves(state),
      h5_moves(state),
      h7_moves(state),
      h9_moves(state),
      h10_moves(state)
    ]
    |> List.flatten()
  end

  defp expended(letter, distance) do
    multiplier = case letter do
      "A" -> 1
      "B" -> 10
      "C" -> 100
      "D" -> 1_000
    end
    multiplier * distance
  end

  defp heuristic_change(letter, initial, curr) do
    case letter do
      "A" -> a_distance(initial) - a_distance(curr)
      "B" -> b_distance(initial) - b_distance(curr)
      "C" -> c_distance(initial) - c_distance(curr)
      "D" -> d_distance(initial) - d_distance(curr)
    end
  end

  defp a_distance(pos) do
    case pos do
      :a0 -> 4
      :a1 -> 5
      :a2 -> 6
      :b0 -> 4
      :b1 -> 5
      :b2 -> 6
      :b3 -> 7
      :c0 -> 6
      :c1 -> 7
      :c2 -> 8
      :c3 -> 9
      :d0 -> 8
      :d1 -> 9
      :d2 -> 10
      :d3 -> 11
      :h0 -> 3
      :h1 -> 2
      :h3 -> 2
      :h5 -> 4
      :h7 -> 6
      :h9 -> 8
      :h10 -> 9
    end
  end

  defp b_distance(pos) do
    case pos do
      :a0 -> 40
      :a1 -> 50
      :a2 -> 60
      :a3 -> 70
      :b0 -> 40
      :b1 -> 50
      :b2 -> 60
      :c0 -> 40
      :c1 -> 50
      :c2 -> 60
      :c3 -> 70
      :d0 -> 60
      :d1 -> 70
      :d2 -> 80
      :d3 -> 90
      :h0 -> 50
      :h1 -> 40
      :h3 -> 20
      :h5 -> 20
      :h7 -> 40
      :h9 -> 60
      :h10 -> 70
    end
  end

  defp c_distance(pos) do
    case pos do
      :a0 -> 600
      :a1 -> 700
      :a2 -> 800
      :a3 -> 900
      :b0 -> 400
      :b1 -> 500
      :b2 -> 600
      :b3 -> 700
      :c0 -> 400
      :c1 -> 500
      :c2 -> 600
      :d0 -> 400
      :d1 -> 500
      :d2 -> 600
      :d3 -> 700
      :h0 -> 700
      :h1 -> 600
      :h3 -> 400
      :h5 -> 200
      :h7 -> 200
      :h9 -> 400
      :h10 -> 500
    end
  end

  defp d_distance(pos) do
    case pos do
      :a0 -> 8_000
      :a1 -> 9_000
      :a2 -> 10_000
      :a3 -> 11_000
      :b0 -> 6_000
      :b1 -> 7_000
      :b2 -> 8_000
      :b3 -> 9_000
      :c0 -> 4_000
      :c1 -> 5_000
      :c2 -> 6_000
      :c3 -> 7_000
      :d0 -> 4_000
      :d1 -> 5_000
      :d2 -> 6_000
      :h0 -> 9_000
      :h1 -> 8_000
      :h3 -> 6_000
      :h5 -> 4_000
      :h7 -> 2_000
      :h9 -> 2_000
      :h10 -> 3_000
    end
  end

  defp a0_moves(%{a0: "."}), do: []
  defp a0_moves(%{a0: "A", a1: "A", a2: "A", a3: "A"}), do: []
  defp a0_moves(%{a0: a0, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." ->
        move =
          state
          |> Map.put(:a0, ".")
          |> Map.put(:h0, a0)
          |> Map.put(:energy, energy + expended(a0, 3))
          |> Map.put(:heuristic, heuristic - heuristic_change(a0, :a0, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." ->
        move =
          state
          |> Map.put(:a0, ".")
          |> Map.put(:h1, a0)
          |> Map.put(:energy, energy + expended(a0, 2))
          |> Map.put(:heuristic, heuristic - heuristic_change(a0, :a0, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." ->
        move =
          state
          |> Map.put(:a0, ".")
          |> Map.put(:h3, a0)
          |> Map.put(:energy, energy + expended(a0, 2))
          |> Map.put(:heuristic, heuristic - heuristic_change(a0, :a0, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a0, ".")
          |> Map.put(:h5, a0)
          |> Map.put(:energy, energy + expended(a0, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(a0, :a0, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a0, ".")
          |> Map.put(:h7, a0)
          |> Map.put(:energy, energy + expended(a0, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(a0, :a0, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a0, ".")
          |> Map.put(:h9, a0)
          |> Map.put(:energy, energy + expended(a0, 8))
          |> Map.put(:heuristic, heuristic - heuristic_change(a0, :a0, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a0, ".")
          |> Map.put(:h10, a0)
          |> Map.put(:energy, energy + expended(a0, 9))
          |> Map.put(:heuristic, heuristic - heuristic_change(a0, :a0, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp a1_moves(%{a1: "."}), do: []
  defp a1_moves(%{a1: "A", a2: "A", a3: "A"}), do: []
  defp a1_moves(%{a0: a0}) when a0 != ".", do: []
  defp a1_moves(%{a1: a1, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." ->
        move =
          state
          |> Map.put(:a1, ".")
          |> Map.put(:h0, a1)
          |> Map.put(:energy, energy + expended(a1, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(a1, :a1, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." ->
        move =
          state
          |> Map.put(:a1, ".")
          |> Map.put(:h1, a1)
          |> Map.put(:energy, energy + expended(a1, 3))
          |> Map.put(:heuristic, heuristic - heuristic_change(a1, :a1, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." ->
        move =
          state
          |> Map.put(:a1, ".")
          |> Map.put(:h3, a1)
          |> Map.put(:energy, energy + expended(a1, 3))
          |> Map.put(:heuristic, heuristic - heuristic_change(a1, :a1, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a1, ".")
          |> Map.put(:h5, a1)
          |> Map.put(:energy, energy + expended(a1, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(a1, :a1, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a1, ".")
          |> Map.put(:h7, a1)
          |> Map.put(:energy, energy + expended(a1, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(a1, :a1, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a1, ".")
          |> Map.put(:h9, a1)
          |> Map.put(:energy, energy + expended(a1, 9))
          |> Map.put(:heuristic, heuristic - heuristic_change(a1, :a1, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a1, ".")
          |> Map.put(:h10, a1)
          |> Map.put(:energy, energy + expended(a1, 10))
          |> Map.put(:heuristic, heuristic - heuristic_change(a1, :a1, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp a2_moves(%{a2: "."}), do: []
  defp a2_moves(%{a2: "A", a3: "A"}), do: []
  defp a2_moves(%{a0: a0, a1: a1}) when a0 != "." or a1 != ".", do: []
  defp a2_moves(%{a2: a2, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." ->
        move =
          state
          |> Map.put(:a2, ".")
          |> Map.put(:h0, a2)
          |> Map.put(:energy, energy + expended(a2, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(a2, :a2, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." ->
        move =
          state
          |> Map.put(:a2, ".")
          |> Map.put(:h1, a2)
          |> Map.put(:energy, energy + expended(a2, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(a2, :a2, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." ->
        move =
          state
          |> Map.put(:a2, ".")
          |> Map.put(:h3, a2)
          |> Map.put(:energy, energy + expended(a2, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(a2, :a2, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a2, ".")
          |> Map.put(:h5, a2)
          |> Map.put(:energy, energy + expended(a2, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(a2, :a2, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a2, ".")
          |> Map.put(:h7, a2)
          |> Map.put(:energy, energy + expended(a2, 8))
          |> Map.put(:heuristic, heuristic - heuristic_change(a2, :a2, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a2, ".")
          |> Map.put(:h9, a2)
          |> Map.put(:energy, energy + expended(a2, 10))
          |> Map.put(:heuristic, heuristic - heuristic_change(a2, :a2, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a2, ".")
          |> Map.put(:h10, a2)
          |> Map.put(:energy, energy + expended(a2, 11))
          |> Map.put(:heuristic, heuristic - heuristic_change(a2, :a2, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp a3_moves(%{a3: "."}), do: []
  defp a3_moves(%{a3: "A"}), do: []
  defp a3_moves(%{a0: a0, a1: a1, a2: a2}) when a0 != "." or a1 != "." or a2 != ".", do: []
  defp a3_moves(%{a3: a3, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." ->
        move =
          state
          |> Map.put(:a3, ".")
          |> Map.put(:h0, a3)
          |> Map.put(:energy, energy + expended(a3, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(a3, :a3, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." ->
        move =
          state
          |> Map.put(:a3, ".")
          |> Map.put(:h1, a3)
          |> Map.put(:energy, energy + expended(a3, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(a3, :a3, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." ->
        move =
          state
          |> Map.put(:a3, ".")
          |> Map.put(:h3, a3)
          |> Map.put(:energy, energy + expended(a3, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(a3, :a3, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a3, ".")
          |> Map.put(:h5, a3)
          |> Map.put(:energy, energy + expended(a3, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(a3, :a3, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a3, ".")
          |> Map.put(:h7, a3)
          |> Map.put(:energy, energy + expended(a3, 9))
          |> Map.put(:heuristic, heuristic - heuristic_change(a3, :a3, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a3, ".")
          |> Map.put(:h9, a3)
          |> Map.put(:energy, energy + expended(a3, 11))
          |> Map.put(:heuristic, heuristic - heuristic_change(a3, :a3, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." and h5 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:a3, ".")
          |> Map.put(:h10, a3)
          |> Map.put(:energy, energy + expended(a3, 12))
          |> Map.put(:heuristic, heuristic - heuristic_change(a3, :a3, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp b0_moves(%{b0: "."}), do: []
  defp b0_moves(%{b0: "B", b1: "B", b2: "B", b3: "B"}), do: []
  defp b0_moves(%{b0: b0, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:b0, ".")
          |> Map.put(:h0, b0)
          |> Map.put(:energy, energy + expended(b0, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(b0, :b0, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:b0, ".")
          |> Map.put(:h1, b0)
          |> Map.put(:energy, energy + expended(b0, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(b0, :b0, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." ->
        move =
          state
          |> Map.put(:b0, ".")
          |> Map.put(:h3, b0)
          |> Map.put(:energy, energy + expended(b0, 2))
          |> Map.put(:heuristic, heuristic - heuristic_change(b0, :b0, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." ->
        move =
          state
          |> Map.put(:b0, ".")
          |> Map.put(:h5, b0)
          |> Map.put(:energy, energy + expended(b0, 2))
          |> Map.put(:heuristic, heuristic - heuristic_change(b0, :b0, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b0, ".")
          |> Map.put(:h7, b0)
          |> Map.put(:energy, energy + expended(b0, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(b0, :b0, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b0, ".")
          |> Map.put(:h9, b0)
          |> Map.put(:energy, energy + expended(b0, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(b0, :b0, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b0, ".")
          |> Map.put(:h10, b0)
          |> Map.put(:energy, energy + expended(b0, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(b0, :b0, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp b1_moves(%{b1: "."}), do: []
  defp b1_moves(%{b1: "B", b2: "B", b3: "B"}), do: []
  defp b1_moves(%{b0: b0}) when b0 != ".", do: []
  defp b1_moves(%{b1: b1, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:b1, ".")
          |> Map.put(:h0, b1)
          |> Map.put(:energy, energy + expended(b1, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(b1, :b1, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:b1, ".")
          |> Map.put(:h1, b1)
          |> Map.put(:energy, energy + expended(b1, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(b1, :b1, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." ->
        move =
          state
          |> Map.put(:b1, ".")
          |> Map.put(:h3, b1)
          |> Map.put(:energy, energy + expended(b1, 3))
          |> Map.put(:heuristic, heuristic - heuristic_change(b1, :b1, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." ->
        move =
          state
          |> Map.put(:b1, ".")
          |> Map.put(:h5, b1)
          |> Map.put(:energy, energy + expended(b1, 3))
          |> Map.put(:heuristic, heuristic - heuristic_change(b1, :b1, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b1, ".")
          |> Map.put(:h7, b1)
          |> Map.put(:energy, energy + expended(b1, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(b1, :b1, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b1, ".")
          |> Map.put(:h9, b1)
          |> Map.put(:energy, energy + expended(b1, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(b1, :b1, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b1, ".")
          |> Map.put(:h10, b1)
          |> Map.put(:energy, energy + expended(b1, 8))
          |> Map.put(:heuristic, heuristic - heuristic_change(b1, :b1, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp b2_moves(%{b2: "."}), do: []
  defp b2_moves(%{b2: "B", b3: "B"}), do: []
  defp b2_moves(%{b0: b0, b1: b1}) when b0 != "." or b1 != ".", do: []
  defp b2_moves(%{b2: b2, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:b2, ".")
          |> Map.put(:h0, b2)
          |> Map.put(:energy, energy + expended(b2, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(b2, :b2, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:b2, ".")
          |> Map.put(:h1, b2)
          |> Map.put(:energy, energy + expended(b2, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(b2, :b2, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." ->
        move =
          state
          |> Map.put(:b2, ".")
          |> Map.put(:h3, b2)
          |> Map.put(:energy, energy + expended(b2, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(b2, :b2, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." ->
        move =
          state
          |> Map.put(:b2, ".")
          |> Map.put(:h5, b2)
          |> Map.put(:energy, energy + expended(b2, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(b2, :b2, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b2, ".")
          |> Map.put(:h7, b2)
          |> Map.put(:energy, energy + expended(b2, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(b2, :b2, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b2, ".")
          |> Map.put(:h9, b2)
          |> Map.put(:energy, energy + expended(b2, 8))
          |> Map.put(:heuristic, heuristic - heuristic_change(b2, :b2, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b2, ".")
          |> Map.put(:h10, b2)
          |> Map.put(:energy, energy + expended(b2, 9))
          |> Map.put(:heuristic, heuristic - heuristic_change(b2, :b2, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp b3_moves(%{b3: "."}), do: []
  defp b3_moves(%{b3: "B"}), do: []
  defp b3_moves(%{b0: b0, b1: b1, b2: b2}) when b0 != "." or b1 != "." or b2 != ".", do: []
  defp b3_moves(%{b3: b3, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:b3, ".")
          |> Map.put(:h0, b3)
          |> Map.put(:energy, energy + expended(b3, 8))
          |> Map.put(:heuristic, heuristic - heuristic_change(b3, :b3, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." ->
        move =
          state
          |> Map.put(:b3, ".")
          |> Map.put(:h1, b3)
          |> Map.put(:energy, energy + expended(b3, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(b3, :b3, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." ->
        move =
          state
          |> Map.put(:b3, ".")
          |> Map.put(:h3, b3)
          |> Map.put(:energy, energy + expended(b3, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(b3, :b3, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." ->
        move =
          state
          |> Map.put(:b3, ".")
          |> Map.put(:h5, b3)
          |> Map.put(:energy, energy + expended(b3, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(b3, :b3, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b3, ".")
          |> Map.put(:h7, b3)
          |> Map.put(:energy, energy + expended(b3, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(b3, :b3, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b3, ".")
          |> Map.put(:h9, b3)
          |> Map.put(:energy, energy + expended(b3, 9))
          |> Map.put(:heuristic, heuristic - heuristic_change(b3, :b3, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:b3, ".")
          |> Map.put(:h10, b3)
          |> Map.put(:energy, energy + expended(b3, 10))
          |> Map.put(:heuristic, heuristic - heuristic_change(b3, :b3, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp c0_moves(%{c0: "."}), do: []
  defp c0_moves(%{c0: "C", c1: "C", c2: "C", c3: "C"}), do: []
  defp c0_moves(%{c0: c0, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c0, ".")
          |> Map.put(:h0, c0)
          |> Map.put(:energy, energy + expended(c0, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(c0, :c0, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c0, ".")
          |> Map.put(:h1, c0)
          |> Map.put(:energy, energy + expended(c0, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(c0, :c0, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c0, ".")
          |> Map.put(:h3, c0)
          |> Map.put(:energy, energy + expended(c0, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(c0, :c0, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." ->
        move =
          state
          |> Map.put(:c0, ".")
          |> Map.put(:h5, c0)
          |> Map.put(:energy, energy + expended(c0, 2))
          |> Map.put(:heuristic, heuristic - heuristic_change(c0, :c0, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." ->
        move =
          state
          |> Map.put(:c0, ".")
          |> Map.put(:h7, c0)
          |> Map.put(:energy, energy + expended(c0, 2))
          |> Map.put(:heuristic, heuristic - heuristic_change(c0, :c0, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:c0, ".")
          |> Map.put(:h9, c0)
          |> Map.put(:energy, energy + expended(c0, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(c0, :c0, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:c0, ".")
          |> Map.put(:h10, c0)
          |> Map.put(:energy, energy + expended(c0, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(c0, :c0, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp c1_moves(%{c1: "."}), do: []
  defp c1_moves(%{c1: "C", c2: "C", c3: "C"}), do: []
  defp c1_moves(%{c0: c0}) when c0 != ".", do: []
  defp c1_moves(%{c1: c1, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c1, ".")
          |> Map.put(:h0, c1)
          |> Map.put(:energy, energy + expended(c1, 8))
          |> Map.put(:heuristic, heuristic - heuristic_change(c1, :c1, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c1, ".")
          |> Map.put(:h1, c1)
          |> Map.put(:energy, energy + expended(c1, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(c1, :c1, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c1, ".")
          |> Map.put(:h3, c1)
          |> Map.put(:energy, energy + expended(c1, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(c1, :c1, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." ->
        move =
          state
          |> Map.put(:c1, ".")
          |> Map.put(:h5, c1)
          |> Map.put(:energy, energy + expended(c1, 3))
          |> Map.put(:heuristic, heuristic - heuristic_change(c1, :c1, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." ->
        move =
          state
          |> Map.put(:c1, ".")
          |> Map.put(:h7, c1)
          |> Map.put(:energy, energy + expended(c1, 3))
          |> Map.put(:heuristic, heuristic - heuristic_change(c1, :c1, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:c1, ".")
          |> Map.put(:h9, c1)
          |> Map.put(:energy, energy + expended(c1, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(c1, :c1, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:c1, ".")
          |> Map.put(:h10, c1)
          |> Map.put(:energy, energy + expended(c1, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(c1, :c1, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp c2_moves(%{c2: "."}), do: []
  defp c2_moves(%{c2: "C", c3: "C"}), do: []
  defp c2_moves(%{c0: c0, c1: c1}) when c0 != "." or c1 != ".", do: []
  defp c2_moves(%{c2: c2, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c2, ".")
          |> Map.put(:h0, c2)
          |> Map.put(:energy, energy + expended(c2, 9))
          |> Map.put(:heuristic, heuristic - heuristic_change(c2, :c2, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c2, ".")
          |> Map.put(:h1, c2)
          |> Map.put(:energy, energy + expended(c2, 8))
          |> Map.put(:heuristic, heuristic - heuristic_change(c2, :c2, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c2, ".")
          |> Map.put(:h3, c2)
          |> Map.put(:energy, energy + expended(c2, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(c2, :c2, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." ->
        move =
          state
          |> Map.put(:c2, ".")
          |> Map.put(:h5, c2)
          |> Map.put(:energy, energy + expended(c2, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(c2, :c2, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." ->
        move =
          state
          |> Map.put(:c2, ".")
          |> Map.put(:h7, c2)
          |> Map.put(:energy, energy + expended(c2, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(c2, :c2, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:c2, ".")
          |> Map.put(:h9, c2)
          |> Map.put(:energy, energy + expended(c2, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(c2, :c2, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:c2, ".")
          |> Map.put(:h10, c2)
          |> Map.put(:energy, energy + expended(c2, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(c2, :c2, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp c3_moves(%{c3: "."}), do: []
  defp c3_moves(%{c3: "C"}), do: []
  defp c3_moves(%{c0: c0, c1: c1, c2: c2}) when c0 != "." or c1 != "." or c2 != ".", do: []
  defp c3_moves(%{c3: c3, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c3, ".")
          |> Map.put(:h0, c3)
          |> Map.put(:energy, energy + expended(c3, 10))
          |> Map.put(:heuristic, heuristic - heuristic_change(c3, :c3, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c3, ".")
          |> Map.put(:h1, c3)
          |> Map.put(:energy, energy + expended(c3, 9))
          |> Map.put(:heuristic, heuristic - heuristic_change(c3, :c3, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." and h5 == "." ->
        move =
          state
          |> Map.put(:c3, ".")
          |> Map.put(:h3, c3)
          |> Map.put(:energy, energy + expended(c3, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(c3, :c3, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." ->
        move =
          state
          |> Map.put(:c3, ".")
          |> Map.put(:h5, c3)
          |> Map.put(:energy, energy + expended(c3, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(c3, :c3, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." ->
        move =
          state
          |> Map.put(:c3, ".")
          |> Map.put(:h7, c3)
          |> Map.put(:energy, energy + expended(c3, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(c3, :c3, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:c3, ".")
          |> Map.put(:h9, c3)
          |> Map.put(:energy, energy + expended(c3, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(c3, :c3, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:c3, ".")
          |> Map.put(:h10, c3)
          |> Map.put(:energy, energy + expended(c3, 8))
          |> Map.put(:heuristic, heuristic - heuristic_change(c3, :c3, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp d0_moves(%{d0: "."}), do: []
  defp d0_moves(%{d0: "D", d1: "D", d2: "D", d3: "D"}), do: []
  defp d0_moves(%{d0: d0, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d0, ".")
          |> Map.put(:h0, d0)
          |> Map.put(:energy, energy + expended(d0, 9))
          |> Map.put(:heuristic, heuristic - heuristic_change(d0, :d0, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d0, ".")
          |> Map.put(:h1, d0)
          |> Map.put(:energy, energy + expended(d0, 8))
          |> Map.put(:heuristic, heuristic - heuristic_change(d0, :d0, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d0, ".")
          |> Map.put(:h3, d0)
          |> Map.put(:energy, energy + expended(d0, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(d0, :d0, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d0, ".")
          |> Map.put(:h5, d0)
          |> Map.put(:energy, energy + expended(d0, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(d0, :d0, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." ->
        move =
          state
          |> Map.put(:d0, ".")
          |> Map.put(:h7, d0)
          |> Map.put(:energy, energy + expended(d0, 2))
          |> Map.put(:heuristic, heuristic - heuristic_change(d0, :d0, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." ->
        move =
          state
          |> Map.put(:d0, ".")
          |> Map.put(:h9, d0)
          |> Map.put(:energy, energy + expended(d0, 2))
          |> Map.put(:heuristic, heuristic - heuristic_change(d0, :d0, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." ->
        move =
          state
          |> Map.put(:d0, ".")
          |> Map.put(:h10, d0)
          |> Map.put(:energy, energy + expended(d0, 3))
          |> Map.put(:heuristic, heuristic - heuristic_change(d0, :d0, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp d1_moves(%{d1: "."}), do: []
  defp d1_moves(%{d1: "D", d2: "D", d3: "D"}), do: []
  defp d1_moves(%{d0: d0}) when d0 != ".", do: []
  defp d1_moves(%{d1: d1, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d1, ".")
          |> Map.put(:h0, d1)
          |> Map.put(:energy, energy + expended(d1, 10))
          |> Map.put(:heuristic, heuristic - heuristic_change(d1, :d1, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d1, ".")
          |> Map.put(:h1, d1)
          |> Map.put(:energy, energy + expended(d1, 9))
          |> Map.put(:heuristic, heuristic - heuristic_change(d1, :d1, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d1, ".")
          |> Map.put(:h3, d1)
          |> Map.put(:energy, energy + expended(d1, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(d1, :d1, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d1, ".")
          |> Map.put(:h5, d1)
          |> Map.put(:energy, energy + expended(d1, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(d1, :d1, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." ->
        move =
          state
          |> Map.put(:d1, ".")
          |> Map.put(:h7, d1)
          |> Map.put(:energy, energy + expended(d1, 3))
          |> Map.put(:heuristic, heuristic - heuristic_change(d1, :d1, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." ->
        move =
          state
          |> Map.put(:d1, ".")
          |> Map.put(:h9, d1)
          |> Map.put(:energy, energy + expended(d1, 3))
          |> Map.put(:heuristic, heuristic - heuristic_change(d1, :d1, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." ->
        move =
          state
          |> Map.put(:d1, ".")
          |> Map.put(:h10, d1)
          |> Map.put(:energy, energy + expended(d1, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(d1, :d1, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp d2_moves(%{d2: "."}), do: []
  defp d2_moves(%{d2: "D", d3: "D"}), do: []
  defp d2_moves(%{d0: d0, d1: d1}) when d0 != "." or d1 != ".", do: []
  defp d2_moves(%{d2: d2, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d2, ".")
          |> Map.put(:h0, d2)
          |> Map.put(:energy, energy + expended(d2, 11))
          |> Map.put(:heuristic, heuristic - heuristic_change(d2, :d2, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d2, ".")
          |> Map.put(:h1, d2)
          |> Map.put(:energy, energy + expended(d2, 10))
          |> Map.put(:heuristic, heuristic - heuristic_change(d2, :d2, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d2, ".")
          |> Map.put(:h3, d2)
          |> Map.put(:energy, energy + expended(d2, 8))
          |> Map.put(:heuristic, heuristic - heuristic_change(d2, :d2, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d2, ".")
          |> Map.put(:h5, d2)
          |> Map.put(:energy, energy + expended(d2, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(d2, :d2, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." ->
        move =
          state
          |> Map.put(:d2, ".")
          |> Map.put(:h7, d2)
          |> Map.put(:energy, energy + expended(d2, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(d2, :d2, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." ->
        move =
          state
          |> Map.put(:d2, ".")
          |> Map.put(:h9, d2)
          |> Map.put(:energy, energy + expended(d2, 4))
          |> Map.put(:heuristic, heuristic - heuristic_change(d2, :d2, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." ->
        move =
          state
          |> Map.put(:d2, ".")
          |> Map.put(:h10, d2)
          |> Map.put(:energy, energy + expended(d2, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(d2, :d2, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp d3_moves(%{d3: "."}), do: []
  defp d3_moves(%{d3: "D"}), do: []
  defp d3_moves(%{d0: d0, d1: d1, d2: d2}) when d0 != "." or d1 != "." or d2 != ".", do: []
  defp d3_moves(%{d3: d3, h0: h0, h1: h1, h3: h3, h5: h5, h7: h7, h9: h9, h10: h10, energy: energy, heuristic: heuristic} = state) do
    moves = cond do
      h0 == "." and h1 == "." and h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d3, ".")
          |> Map.put(:h0, d3)
          |> Map.put(:energy, energy + expended(d3, 12))
          |> Map.put(:heuristic, heuristic - heuristic_change(d3, :d3, :h0))
        [move]
      true -> []
    end
    moves = cond do
      h1 == "." and h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d3, ".")
          |> Map.put(:h1, d3)
          |> Map.put(:energy, energy + expended(d3, 11))
          |> Map.put(:heuristic, heuristic - heuristic_change(d3, :d3, :h1))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h3 == "." and h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d3, ".")
          |> Map.put(:h3, d3)
          |> Map.put(:energy, energy + expended(d3, 9))
          |> Map.put(:heuristic, heuristic - heuristic_change(d3, :d3, :h3))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h5 == "." and h7 == "." ->
        move =
          state
          |> Map.put(:d3, ".")
          |> Map.put(:h5, d3)
          |> Map.put(:energy, energy + expended(d3, 7))
          |> Map.put(:heuristic, heuristic - heuristic_change(d3, :d3, :h5))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h7 == "." ->
        move =
          state
          |> Map.put(:d3, ".")
          |> Map.put(:h7, d3)
          |> Map.put(:energy, energy + expended(d3, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(d3, :d3, :h7))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h9 == "." ->
        move =
          state
          |> Map.put(:d3, ".")
          |> Map.put(:h9, d3)
          |> Map.put(:energy, energy + expended(d3, 5))
          |> Map.put(:heuristic, heuristic - heuristic_change(d3, :d3, :h9))
        [move | moves]
      true -> moves
    end
    moves = cond do
      h10 == "." and h9 == "." ->
        move =
          state
          |> Map.put(:d3, ".")
          |> Map.put(:h10, d3)
          |> Map.put(:energy, energy + expended(d3, 6))
          |> Map.put(:heuristic, heuristic - heuristic_change(d3, :d3, :h10))
        [move | moves]
      true -> moves
    end
    moves
  end

  defp h0_moves(%{h0: "."}), do: []
  defp h0_moves(%{h0: "A", h1: ".", a0: ".", a1: "A", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:a0, "A")
    |> Map.put(:energy, energy + 3)
    |> Map.put(:heuristic, heuristic - 3)
  end
  defp h0_moves(%{h0: "A", h1: ".", a0: ".", a1: ".", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:a1, "A")
    |> Map.put(:energy, energy + 4)
    |> Map.put(:heuristic, heuristic - 4)
  end
  defp h0_moves(%{h0: "A", h1: ".", a0: ".", a1: ".", a2: ".", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:a2, "A")
    |> Map.put(:energy, energy + 5)
    |> Map.put(:heuristic, heuristic - 5)
  end
  defp h0_moves(%{h0: "A", h1: ".", a0: ".", a1: ".", a2: ".", a3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:a3, "A")
    |> Map.put(:energy, energy + 6)
    |> Map.put(:heuristic, heuristic - 6)
  end
  defp h0_moves(%{h0: "B", h1: ".", h3: ".", b0: ".", b1: "B", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:b0, "B")
    |> Map.put(:energy, energy + 50)
    |> Map.put(:heuristic, heuristic - 50)
  end
  defp h0_moves(%{h0: "B", h1: ".", h3: ".", b0: ".", b1: ".", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:b1, "B")
    |> Map.put(:energy, energy + 60)
    |> Map.put(:heuristic, heuristic - 60)
  end
  defp h0_moves(%{h0: "B", h1: ".", h3: ".", b0: ".", b1: ".", b2: ".", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:b2, "B")
    |> Map.put(:energy, energy + 70)
    |> Map.put(:heuristic, heuristic - 70)
  end
  defp h0_moves(%{h0: "B", h1: ".", h3: ".", b0: ".", b1: ".", b2: ".", b3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:b3, "B")
    |> Map.put(:energy, energy + 80)
    |> Map.put(:heuristic, heuristic - 80)
  end
  defp h0_moves(%{h0: "C", h1: ".", h3: ".", h5: ".", c0: ".", c1: "C", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:c0, "C")
    |> Map.put(:energy, energy + 700)
    |> Map.put(:heuristic, heuristic - 700)
  end
  defp h0_moves(%{h0: "C", h1: ".", h3: ".", h5: ".", c0: ".", c1: ".", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:c1, "C")
    |> Map.put(:energy, energy + 800)
    |> Map.put(:heuristic, heuristic - 800)
  end
  defp h0_moves(%{h0: "C", h1: ".", h3: ".", h5: ".", c0: ".", c1: ".", c2: ".", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:c2, "C")
    |> Map.put(:energy, energy + 900)
    |> Map.put(:heuristic, heuristic - 900)
  end
  defp h0_moves(%{h0: "C", h1: ".", h3: ".", h5: ".", c0: ".", c1: ".", c2: ".", c3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:c3, "C")
    |> Map.put(:energy, energy + 1_000)
    |> Map.put(:heuristic, heuristic - 1_000)
  end
  defp h0_moves(%{h0: "D", h1: ".", h3: ".", h5: ".", h7: ".", d0: ".", d1: "D", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:d0, "D")
    |> Map.put(:energy, energy + 9_000)
    |> Map.put(:heuristic, heuristic - 9_000)
  end
  defp h0_moves(%{h0: "D", h1: ".", h3: ".", h5: ".", h7: ".", d0: ".", d1: ".", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:d1, "D")
    |> Map.put(:energy, energy + 10_000)
    |> Map.put(:heuristic, heuristic - 10_000)
  end
  defp h0_moves(%{h0: "D", h1: ".", h3: ".", h5: ".", h7: ".", d0: ".", d1: ".", d2: ".", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:d2, "D")
    |> Map.put(:energy, energy + 11_000)
    |> Map.put(:heuristic, heuristic - 11_000)
  end
  defp h0_moves(%{h0: "D", h1: ".", h3: ".", h5: ".", h7: ".", d0: ".", d1: ".", d2: ".", d3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h0, ".")
    |> Map.put(:d3, "D")
    |> Map.put(:energy, energy + 12_000)
    |> Map.put(:heuristic, heuristic - 12_000)
  end
  defp h0_moves(_state), do: []

  defp h1_moves(%{h1: "."}), do: []
  defp h1_moves(%{h1: "A", a0: ".", a1: "A", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:a0, "A")
    |> Map.put(:energy, energy + 2)
    |> Map.put(:heuristic, heuristic - 2)
  end
  defp h1_moves(%{h1: "A", a0: ".", a1: ".", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:a1, "A")
    |> Map.put(:energy, energy + 3)
    |> Map.put(:heuristic, heuristic - 3)
  end
  defp h1_moves(%{h1: "A", a0: ".", a1: ".", a2: ".", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:a2, "A")
    |> Map.put(:energy, energy + 4)
    |> Map.put(:heuristic, heuristic - 4)
  end
  defp h1_moves(%{h1: "A", a0: ".", a1: ".", a2: ".", a3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:a3, "A")
    |> Map.put(:energy, energy + 5)
    |> Map.put(:heuristic, heuristic - 5)
  end
  defp h1_moves(%{h1: "B", h3: ".", b0: ".", b1: "B", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:b0, "B")
    |> Map.put(:energy, energy + 40)
    |> Map.put(:heuristic, heuristic - 40)
  end
  defp h1_moves(%{h1: "B", h3: ".", b0: ".", b1: ".", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:b1, "B")
    |> Map.put(:energy, energy + 50)
    |> Map.put(:heuristic, heuristic - 50)
  end
  defp h1_moves(%{h1: "B", h3: ".", b0: ".", b1: ".", b2: ".", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:b2, "B")
    |> Map.put(:energy, energy + 60)
    |> Map.put(:heuristic, heuristic - 60)
  end
  defp h1_moves(%{h1: "B", h3: ".", b0: ".", b1: ".", b2: ".", b3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:b3, "B")
    |> Map.put(:energy, energy + 70)
    |> Map.put(:heuristic, heuristic - 70)
  end
  defp h1_moves(%{h1: "C", h3: ".", h5: ".", c0: ".", c1: "C", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:c0, "C")
    |> Map.put(:energy, energy + 600)
    |> Map.put(:heuristic, heuristic - 600)
  end
  defp h1_moves(%{h1: "C", h3: ".", h5: ".", c0: ".", c1: ".", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:c1, "C")
    |> Map.put(:energy, energy + 700)
    |> Map.put(:heuristic, heuristic - 700)
  end
  defp h1_moves(%{h1: "C", h3: ".", h5: ".", c0: ".", c1: ".", c2: ".", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:c2, "C")
    |> Map.put(:energy, energy + 800)
    |> Map.put(:heuristic, heuristic - 800)
  end
  defp h1_moves(%{h1: "C", h3: ".", h5: ".", c0: ".", c1: ".", c2: ".", c3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:c3, "C")
    |> Map.put(:energy, energy + 900)
    |> Map.put(:heuristic, heuristic - 900)
  end
  defp h1_moves(%{h1: "D", h3: ".", h5: ".", h7: ".", d0: ".", d1: "D", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:d0, "D")
    |> Map.put(:energy, energy + 8_000)
    |> Map.put(:heuristic, heuristic - 8_000)
  end
  defp h1_moves(%{h1: "D", h3: ".", h5: ".", h7: ".", d0: ".", d1: ".", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:d1, "D")
    |> Map.put(:energy, energy + 9_000)
    |> Map.put(:heuristic, heuristic - 9_000)
  end
  defp h1_moves(%{h1: "D", h3: ".", h5: ".", h7: ".", d0: ".", d1: ".", d2: ".", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:d2, "D")
    |> Map.put(:energy, energy + 10_000)
    |> Map.put(:heuristic, heuristic - 10_000)
  end
  defp h1_moves(%{h1: "D", h3: ".", h5: ".", h7: ".", d0: ".", d1: ".", d2: ".", d3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h1, ".")
    |> Map.put(:d3, "D")
    |> Map.put(:energy, energy + 11_000)
    |> Map.put(:heuristic, heuristic - 11_000)
  end
  defp h1_moves(_state), do: []

  defp h3_moves(%{h3: "."}), do: []
  defp h3_moves(%{h3: "A", a0: ".", a1: "A", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:a0, "A")
    |> Map.put(:energy, energy + 2)
    |> Map.put(:heuristic, heuristic - 2)
  end
  defp h3_moves(%{h3: "A", a0: ".", a1: ".", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:a1, "A")
    |> Map.put(:energy, energy + 3)
    |> Map.put(:heuristic, heuristic - 3)
  end
  defp h3_moves(%{h3: "A", a0: ".", a1: ".", a2: ".", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:a2, "A")
    |> Map.put(:energy, energy + 4)
    |> Map.put(:heuristic, heuristic - 4)
  end
  defp h3_moves(%{h3: "A", a0: ".", a1: ".", a2: ".", a3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:a3, "A")
    |> Map.put(:energy, energy + 5)
    |> Map.put(:heuristic, heuristic - 5)
  end
  defp h3_moves(%{h3: "B", b0: ".", b1: "B", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:b0, "B")
    |> Map.put(:energy, energy + 20)
    |> Map.put(:heuristic, heuristic - 20)
  end
  defp h3_moves(%{h3: "B", b0: ".", b1: ".", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:b1, "B")
    |> Map.put(:energy, energy + 30)
    |> Map.put(:heuristic, heuristic - 30)
  end
  defp h3_moves(%{h3: "B", b0: ".", b1: ".", b2: ".", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:b2, "B")
    |> Map.put(:energy, energy + 40)
    |> Map.put(:heuristic, heuristic - 40)
  end
  defp h3_moves(%{h3: "B", b0: ".", b1: ".", b2: ".", b3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:b3, "B")
    |> Map.put(:energy, energy + 50)
    |> Map.put(:heuristic, heuristic - 50)
  end
  defp h3_moves(%{h3: "C", h5: ".", c0: ".", c1: "C", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:c0, "C")
    |> Map.put(:energy, energy + 400)
    |> Map.put(:heuristic, heuristic - 400)
  end
  defp h3_moves(%{h3: "C", h5: ".", c0: ".", c1: ".", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:c1, "C")
    |> Map.put(:energy, energy + 500)
    |> Map.put(:heuristic, heuristic - 500)
  end
  defp h3_moves(%{h3: "C", h5: ".", c0: ".", c1: ".", c2: ".", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:c2, "C")
    |> Map.put(:energy, energy + 600)
    |> Map.put(:heuristic, heuristic - 600)
  end
  defp h3_moves(%{h3: "C", h5: ".", c0: ".", c1: ".", c2: ".", c3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:c3, "C")
    |> Map.put(:energy, energy + 700)
    |> Map.put(:heuristic, heuristic - 700)
  end
  defp h3_moves(%{h3: "D", h5: ".", h7: ".", d0: ".", d1: "D", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:d0, "D")
    |> Map.put(:energy, energy + 6_000)
    |> Map.put(:heuristic, heuristic - 6_000)
  end
  defp h3_moves(%{h3: "D", h5: ".", h7: ".", d0: ".", d1: ".", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:d1, "D")
    |> Map.put(:energy, energy + 7_000)
    |> Map.put(:heuristic, heuristic - 7_000)
  end
  defp h3_moves(%{h3: "D", h5: ".", h7: ".", d0: ".", d1: ".", d2: ".", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:d2, "D")
    |> Map.put(:energy, energy + 8_000)
    |> Map.put(:heuristic, heuristic - 8_000)
  end
  defp h3_moves(%{h3: "D", h5: ".", h7: ".", d0: ".", d1: ".", d2: ".", d3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h3, ".")
    |> Map.put(:d3, "D")
    |> Map.put(:energy, energy + 9_000)
    |> Map.put(:heuristic, heuristic - 9_000)
  end
  defp h3_moves(_state), do: []

  defp h5_moves(%{h5: "."}), do: []
  defp h5_moves(%{h5: "A", h3: ".", a0: ".", a1: "A", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:a0, "A")
    |> Map.put(:energy, energy + 4)
    |> Map.put(:heuristic, heuristic - 4)
  end
  defp h5_moves(%{h5: "A", h3: ".", a0: ".", a1: ".", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:a1, "A")
    |> Map.put(:energy, energy + 5)
    |> Map.put(:heuristic, heuristic - 5)
  end
  defp h5_moves(%{h5: "A", h3: ".", a0: ".", a1: ".", a2: ".", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:a2, "A")
    |> Map.put(:energy, energy + 6)
    |> Map.put(:heuristic, heuristic - 6)
  end
  defp h5_moves(%{h5: "A", h3: ".", a0: ".", a1: ".", a2: ".", a3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:a3, "A")
    |> Map.put(:energy, energy + 7)
    |> Map.put(:heuristic, heuristic - 7)
  end
  defp h5_moves(%{h5: "B", b0: ".", b1: "B", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:b0, "B")
    |> Map.put(:energy, energy + 20)
    |> Map.put(:heuristic, heuristic - 20)
  end
  defp h5_moves(%{h5: "B", b0: ".", b1: ".", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:b1, "B")
    |> Map.put(:energy, energy + 30)
    |> Map.put(:heuristic, heuristic - 30)
  end
  defp h5_moves(%{h5: "B", b0: ".", b1: ".", b2: ".", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:b2, "B")
    |> Map.put(:energy, energy + 40)
    |> Map.put(:heuristic, heuristic - 40)
  end
  defp h5_moves(%{h5: "B", b0: ".", b1: ".", b2: ".", b3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:b3, "B")
    |> Map.put(:energy, energy + 50)
    |> Map.put(:heuristic, heuristic - 50)
  end
  defp h5_moves(%{h5: "C", c0: ".", c1: "C", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:c0, "C")
    |> Map.put(:energy, energy + 200)
    |> Map.put(:heuristic, heuristic - 200)
  end
  defp h5_moves(%{h5: "C", c0: ".", c1: ".", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:c1, "C")
    |> Map.put(:energy, energy + 300)
    |> Map.put(:heuristic, heuristic - 300)
  end
  defp h5_moves(%{h5: "C", c0: ".", c1: ".", c2: ".", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:c2, "C")
    |> Map.put(:energy, energy + 400)
    |> Map.put(:heuristic, heuristic - 400)
  end
  defp h5_moves(%{h5: "C", c0: ".", c1: ".", c2: ".", c3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:c3, "C")
    |> Map.put(:energy, energy + 500)
    |> Map.put(:heuristic, heuristic - 500)
  end
  defp h5_moves(%{h5: "D", h7: ".", d0: ".", d1: "D", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:d0, "D")
    |> Map.put(:energy, energy + 4_000)
    |> Map.put(:heuristic, heuristic - 4_000)
  end
  defp h5_moves(%{h5: "D", h7: ".", d0: ".", d1: ".", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:d1, "D")
    |> Map.put(:energy, energy + 5_000)
    |> Map.put(:heuristic, heuristic - 5_000)
  end
  defp h5_moves(%{h5: "D", h7: ".", d0: ".", d1: ".", d2: ".", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:d2, "D")
    |> Map.put(:energy, energy + 6_000)
    |> Map.put(:heuristic, heuristic - 6_000)
  end
  defp h5_moves(%{h5: "D", h7: ".", d0: ".", d1: ".", d2: ".", d3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h5, ".")
    |> Map.put(:d3, "D")
    |> Map.put(:energy, energy + 7_000)
    |> Map.put(:heuristic, heuristic - 7_000)
  end
  defp h5_moves(_state), do: []

  defp h7_moves(%{h7: "."}), do: []
  defp h7_moves(%{h7: "A", h5: ".", h3: ".", a0: ".", a1: "A", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:a0, "A")
    |> Map.put(:energy, energy + 6)
    |> Map.put(:heuristic, heuristic - 6)
  end
  defp h7_moves(%{h7: "A", h5: ".", h3: ".", a0: ".", a1: ".", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:a1, "A")
    |> Map.put(:energy, energy + 7)
    |> Map.put(:heuristic, heuristic - 7)
  end
  defp h7_moves(%{h7: "A", h5: ".", h3: ".", a0: ".", a1: ".", a2: ".", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:a2, "A")
    |> Map.put(:energy, energy + 8)
    |> Map.put(:heuristic, heuristic - 8)
  end
  defp h7_moves(%{h7: "A", h5: ".", h3: ".", a0: ".", a1: ".", a2: ".", a3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:a3, "A")
    |> Map.put(:energy, energy + 9)
    |> Map.put(:heuristic, heuristic - 9)
  end
  defp h7_moves(%{h7: "B", h5: ".", b0: ".", b1: "B", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:b0, "B")
    |> Map.put(:energy, energy + 40)
    |> Map.put(:heuristic, heuristic - 40)
  end
  defp h7_moves(%{h7: "B", h5: ".", b0: ".", b1: ".", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:b1, "B")
    |> Map.put(:energy, energy + 50)
    |> Map.put(:heuristic, heuristic - 50)
  end
  defp h7_moves(%{h7: "B", h5: ".", b0: ".", b1: ".", b2: ".", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:b2, "B")
    |> Map.put(:energy, energy + 60)
    |> Map.put(:heuristic, heuristic - 60)
  end
  defp h7_moves(%{h7: "B", h5: ".", b0: ".", b1: ".", b2: ".", b3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:b3, "B")
    |> Map.put(:energy, energy + 70)
    |> Map.put(:heuristic, heuristic - 70)
  end
  defp h7_moves(%{h7: "C", c0: ".", c1: "C", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:c0, "C")
    |> Map.put(:energy, energy + 200)
    |> Map.put(:heuristic, heuristic - 200)
  end
  defp h7_moves(%{h7: "C", c0: ".", c1: ".", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:c1, "C")
    |> Map.put(:energy, energy + 300)
    |> Map.put(:heuristic, heuristic - 300)
  end
  defp h7_moves(%{h7: "C", c0: ".", c1: ".", c2: ".", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:c2, "C")
    |> Map.put(:energy, energy + 400)
    |> Map.put(:heuristic, heuristic - 400)
  end
  defp h7_moves(%{h7: "C", c0: ".", c1: ".", c2: ".", c3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:c2, "C")
    |> Map.put(:energy, energy + 500)
    |> Map.put(:heuristic, heuristic - 500)
  end
  defp h7_moves(%{h7: "D", d0: ".", d1: "D", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:d0, "D")
    |> Map.put(:energy, energy + 2_000)
    |> Map.put(:heuristic, heuristic - 2_000)
  end
  defp h7_moves(%{h7: "D", d0: ".", d1: ".", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:d1, "D")
    |> Map.put(:energy, energy + 3_000)
    |> Map.put(:heuristic, heuristic - 3_000)
  end
  defp h7_moves(%{h7: "D", d0: ".", d1: ".", d2: ".", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:d2, "D")
    |> Map.put(:energy, energy + 4_000)
    |> Map.put(:heuristic, heuristic - 4_000)
  end
  defp h7_moves(%{h7: "D", d0: ".", d1: ".", d2: ".", d3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h7, ".")
    |> Map.put(:d3, "D")
    |> Map.put(:energy, energy + 5_000)
    |> Map.put(:heuristic, heuristic - 5_000)
  end
  defp h7_moves(_state), do: []

  defp h9_moves(%{h9: "."}), do: []
  defp h9_moves(%{h9: "A", h7: ".", h5: ".", h3: ".", a0: ".", a1: "A", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:a0, "A")
    |> Map.put(:energy, energy + 8)
    |> Map.put(:heuristic, heuristic - 8)
  end
  defp h9_moves(%{h9: "A", h7: ".", h5: ".", h3: ".", a0: ".", a1: ".", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:a1, "A")
    |> Map.put(:energy, energy + 9)
    |> Map.put(:heuristic, heuristic - 9)
  end
  defp h9_moves(%{h9: "A", h7: ".", h5: ".", h3: ".", a0: ".", a1: ".", a2: ".", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:a2, "A")
    |> Map.put(:energy, energy + 10)
    |> Map.put(:heuristic, heuristic - 10)
  end
  defp h9_moves(%{h9: "A", h7: ".", h5: ".", h3: ".", a0: ".", a1: ".", a2: ".", a3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:a3, "A")
    |> Map.put(:energy, energy + 11)
    |> Map.put(:heuristic, heuristic - 11)
  end
  defp h9_moves(%{h9: "B", h7: ".", h5: ".", b0: ".", b1: "B", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:b0, "B")
    |> Map.put(:energy, energy + 60)
    |> Map.put(:heuristic, heuristic - 60)
  end
  defp h9_moves(%{h9: "B", h7: ".", h5: ".", b0: ".", b1: ".", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:b1, "B")
    |> Map.put(:energy, energy + 70)
    |> Map.put(:heuristic, heuristic - 70)
  end
  defp h9_moves(%{h9: "B", h7: ".", h5: ".", b0: ".", b1: ".", b2: ".", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:b2, "B")
    |> Map.put(:energy, energy + 80)
    |> Map.put(:heuristic, heuristic - 80)
  end
  defp h9_moves(%{h9: "B", h7: ".", h5: ".", b0: ".", b1: ".", b2: ".", b3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:b3, "B")
    |> Map.put(:energy, energy + 90)
    |> Map.put(:heuristic, heuristic - 90)
  end
  defp h9_moves(%{h9: "C", h7: ".", c0: ".", c1: "C", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:c0, "C")
    |> Map.put(:energy, energy + 400)
    |> Map.put(:heuristic, heuristic - 400)
  end
  defp h9_moves(%{h9: "C", h7: ".", c0: ".", c1: ".", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:c1, "C")
    |> Map.put(:energy, energy + 500)
    |> Map.put(:heuristic, heuristic - 500)
  end
  defp h9_moves(%{h9: "C", h7: ".", c0: ".", c1: ".", c2: ".", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:c2, "C")
    |> Map.put(:energy, energy + 600)
    |> Map.put(:heuristic, heuristic - 600)
  end
  defp h9_moves(%{h9: "C", h7: ".", c0: ".", c1: ".", c2: ".", c3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:c3, "C")
    |> Map.put(:energy, energy + 700)
    |> Map.put(:heuristic, heuristic - 700)
  end
  defp h9_moves(%{h9: "D", d0: ".", d1: "D", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:d0, "D")
    |> Map.put(:energy, energy + 2_000)
    |> Map.put(:heuristic, heuristic - 2_000)
  end
  defp h9_moves(%{h9: "D", d0: ".", d1: ".", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:d1, "D")
    |> Map.put(:energy, energy + 3_000)
    |> Map.put(:heuristic, heuristic - 3_000)
  end
  defp h9_moves(%{h9: "D", d0: ".", d1: ".", d2: ".", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:d2, "D")
    |> Map.put(:energy, energy + 4_000)
    |> Map.put(:heuristic, heuristic - 4_000)
  end
  defp h9_moves(%{h9: "D", d0: ".", d1: ".", d2: ".", d3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h9, ".")
    |> Map.put(:d3, "D")
    |> Map.put(:energy, energy + 5_000)
    |> Map.put(:heuristic, heuristic - 5_000)
  end
  defp h9_moves(_state), do: []

  defp h10_moves(%{h10: "."}), do: []
  defp h10_moves(%{h10: "A", h9: ".", h7: ".", h5: ".", h3: ".", a0: ".", a1: "A", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:a0, "A")
    |> Map.put(:energy, energy + 9)
    |> Map.put(:heuristic, heuristic - 9)
  end
  defp h10_moves(%{h10: "A", h9: ".", h7: ".", h5: ".", h3: ".", a0: ".", a1: ".", a2: "A", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:a1, "A")
    |> Map.put(:energy, energy + 10)
    |> Map.put(:heuristic, heuristic - 10)
  end
  defp h10_moves(%{h10: "A", h9: ".", h7: ".", h5: ".", h3: ".", a0: ".", a1: ".", a2: ".", a3: "A", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:a2, "A")
    |> Map.put(:energy, energy + 11)
    |> Map.put(:heuristic, heuristic - 11)
  end
  defp h10_moves(%{h10: "A", h9: ".", h7: ".", h5: ".", h3: ".", a0: ".", a1: ".", a2: ".", a3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:a3, "A")
    |> Map.put(:energy, energy + 12)
    |> Map.put(:heuristic, heuristic - 12)
  end
  defp h10_moves(%{h10: "B", h9: ".", h7: ".", h5: ".", b0: ".", b1: "B", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:b0, "B")
    |> Map.put(:energy, energy + 70)
    |> Map.put(:heuristic, heuristic - 70)
  end
  defp h10_moves(%{h10: "B", h9: ".", h7: ".", h5: ".", b0: ".", b1: ".", b2: "B", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:b1, "B")
    |> Map.put(:energy, energy + 80)
    |> Map.put(:heuristic, heuristic - 80)
  end
  defp h10_moves(%{h10: "B", h9: ".", h7: ".", h5: ".", b0: ".", b1: ".", b2: ".", b3: "B", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:b2, "B")
    |> Map.put(:energy, energy + 90)
    |> Map.put(:heuristic, heuristic - 90)
  end
  defp h10_moves(%{h10: "B", h9: ".", h7: ".", h5: ".", b0: ".", b1: ".", b2: ".", b3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:b3, "B")
    |> Map.put(:energy, energy + 100)
    |> Map.put(:heuristic, heuristic - 100)
  end
  defp h10_moves(%{h10: "C", h9: ".", h7: ".", c0: ".", c1: "C", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:c0, "C")
    |> Map.put(:energy, energy + 500)
    |> Map.put(:heuristic, heuristic - 500)
  end
  defp h10_moves(%{h10: "C", h9: ".", h7: ".", c0: ".", c1: ".", c2: "C", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:c1, "C")
    |> Map.put(:energy, energy + 600)
    |> Map.put(:heuristic, heuristic - 600)
  end
  defp h10_moves(%{h10: "C", h9: ".", h7: ".", c0: ".", c1: ".", c2: ".", c3: "C", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:c2, "C")
    |> Map.put(:energy, energy + 700)
    |> Map.put(:heuristic, heuristic - 700)
  end
  defp h10_moves(%{h10: "C", h9: ".", h7: ".", c0: ".", c1: ".", c2: ".", c3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:c3, "C")
    |> Map.put(:energy, energy + 800)
    |> Map.put(:heuristic, heuristic - 800)
  end
  defp h10_moves(%{h10: "D", h9: ".", d0: ".", d1: "D", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:d0, "D")
    |> Map.put(:energy, energy + 3_000)
    |> Map.put(:heuristic, heuristic - 3_000)
  end
  defp h10_moves(%{h10: "D", h9: ".", d0: ".", d1: ".", d2: "D", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:d1, "D")
    |> Map.put(:energy, energy + 4_000)
    |> Map.put(:heuristic, heuristic - 4_000)
  end
  defp h10_moves(%{h10: "D", h9: ".", d0: ".", d1: ".", d2: ".", d3: "D", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:d2, "D")
    |> Map.put(:energy, energy + 5_000)
    |> Map.put(:heuristic, heuristic - 5_000)
  end
  defp h10_moves(%{h10: "D", h9: ".", d0: ".", d1: ".", d2: ".", d3: ".", energy: energy, heuristic: heuristic} = state) do
    state
    |> Map.put(:h10, ".")
    |> Map.put(:d3, "D")
    |> Map.put(:energy, energy + 6_000)
    |> Map.put(:heuristic, heuristic - 6_000)
  end
  defp h10_moves(_state), do: []
end
