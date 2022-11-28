defmodule Day18 do
  @moduledoc """
  Advent of Code 2021 Day 18
  """

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x -> convert_to_snailfish_number(x, 1) end)
  end

  def convert_to_snailfish_number(str, depth) do
    index =
      str
      |> String.graphemes()
      |> find_splitting_index(0, 0)

    case index do
      :integer -> %{magnitude: String.to_integer(str), depth: depth}
      num ->
        {left_str, right_str} = String.split_at(str, num)

        left_len = String.length(left_str)
        clean_left_str = String.slice(left_str, 1, left_len - 1)

        right_len = String.length(right_str)
        clean_right_str = String.slice(right_str, 1, right_len - 2)

        left = convert_to_snailfish_number(clean_left_str, depth + 1)
        right = convert_to_snailfish_number(clean_right_str, depth + 1)

        %{left: left, right: right, depth: depth}
    end
  end

  defp find_splitting_index([], _count, _index), do: :integer
  defp find_splitting_index([head | tail], count, index) do
    case head do
      "[" -> find_splitting_index(tail, count + 1, index + 1)
      "]" -> find_splitting_index(tail, count - 1, index + 1)
      "," ->
        case count do
          1 -> index
          _ -> find_splitting_index(tail, count, index + 1)
        end
      _ -> find_splitting_index(tail, count, index + 1)
    end
  end

  @doc """
  Day 18 Part 1

  Find magnitude of final sum
  """
  def find_magnitude(snailfish_numbers) do
    snailfish_numbers
    |> sum_snailfish_numbers()
    |> calculate_magnitude()
  end

  defp sum_snailfish_numbers([snailfish_number]), do: snailfish_number
  defp sum_snailfish_numbers([first | [second | tail]]) do
    sum = sum_two(first, second)
    sum_snailfish_numbers([sum | tail])
  end

  defp sum_two(first, second) do
    %{left: first, right: second, depth: 0}
    |> increase_depths()
    |> reduce()
  end

  defp increase_depths(%{magnitude: _m, depth: depth} = node), do: Map.put(node, :depth, depth + 1)
  defp increase_depths(%{left: left, right: right, depth: depth} = node) do
    node
    |> Map.put(:depth, depth + 1)
    |> Map.put(:left, increase_depths(left))
    |> Map.put(:right, increase_depths(right))
  end

  defp reduce(snailfish_number) do
    case explode(snailfish_number) do
      :valid ->
        case split(snailfish_number) do
          :valid -> snailfish_number
          sn -> reduce(sn)
        end
      sn -> reduce(sn)
    end
  end

  defp explode(snailfish_number) do
    case find_explosion_point(snailfish_number) do
      :nope -> :valid
      {path, {left_num, right_num}} ->
        snailfish_number
        |> put_in(path, %{magnitude: 0, depth: 5})
        |> explode_left(path, left_num)
        |> explode_right(path, right_num)
    end
  end

  defp find_explosion_point(%{magnitude: _m}), do: :nope
  defp find_explosion_point(%{depth: 5, left: left, right: right}) do
    {[], {left[:magnitude], right[:magnitude]}}
  end
  defp find_explosion_point(%{left: left, right: right}) do
    case find_explosion_point(left) do
      :nope ->
        case find_explosion_point(right) do
          :nope -> :nope
          {p, exploder} -> {[:right | p], exploder}
        end
      {p, exploder} -> {[:left | p], exploder}
    end
  end

  defp explode_left(snailfish_number, path, num) do
    reverse_path = Enum.reverse(path)
    case Enum.find_index(reverse_path, fn x -> x == :right end) do
      nil -> snailfish_number
      ix ->
        new_reverse_path = [:left | Enum.slice(reverse_path, ix + 1, length(path))]
        new_path = Enum.reverse(new_reverse_path)
        {final_path, value} = final_path(snailfish_number, new_path, :right)
        put_in(snailfish_number, final_path, value + num)
    end
  end

  defp explode_right(snailfish_number, path, num) do
    reverse_path = Enum.reverse(path)
    case Enum.find_index(reverse_path, fn x -> x == :left end) do
      nil -> snailfish_number
      ix ->
        new_reverse_path = [:right | Enum.slice(reverse_path, ix + 1, length(path))]
        new_path = Enum.reverse(new_reverse_path)
        {final_path, value} = final_path(snailfish_number, new_path, :left)
        put_in(snailfish_number, final_path, value + num)
    end
  end

  defp final_path(%{magnitude: magnitude}, _path, _default), do: {[:magnitude], magnitude}
  defp final_path(node, path, default) do
    case path do
      [head | tail] ->
        {new_path, value} = final_path(node[head], tail, default)
        {[head | new_path], value}

      [] ->
        {new_path, value} = final_path(node[default], [], default)
        {[default | new_path], value}
    end
  end

  defp split(snailfish_number) do
    case find_split_point(snailfish_number) do
      :nope -> :valid
      {path, {magnitude, depth}} ->
        left_num = div(magnitude, 2)
        right_num = case rem(magnitude, 2) do
          0 -> left_num
          _ -> left_num + 1
        end
        left = %{depth: depth + 1, magnitude: left_num}
        right = %{depth: depth + 1, magnitude: right_num}
        node = %{depth: depth, left: left, right: right}
        put_in(snailfish_number, path, node)
    end
  end

  defp find_split_point(%{magnitude: magnitude}) when magnitude < 10, do: :nope
  defp find_split_point(%{magnitude: magnitude, depth: depth}), do: {[], {magnitude, depth}}
  defp find_split_point(%{left: left, right: right}) do
    case find_split_point(left) do
      :nope ->
        case find_split_point(right) do
          :nope -> :nope
          {p, splitter} -> {[:right | p], splitter}
        end
      {p, splitter} -> {[:left | p], splitter}
    end
  end

  defp calculate_magnitude(%{magnitude: magnitude}), do: magnitude
  defp calculate_magnitude(%{left: left, right: right}) do
    (3 * calculate_magnitude(left)) + (2 * calculate_magnitude(right))
  end

  @doc """
  Day 18 Part 2

  Find largest magnitude of any two numbers added together
  """
  def find_largest_magnitude(snailfish_numbers) do
    calculate_largest_magnitude(snailfish_numbers, snailfish_numbers, snailfish_numbers, 0)
  end

  defp calculate_largest_magnitude([], _list2, _original, magnitude), do: magnitude
  defp calculate_largest_magnitude([_head | tail], [], original, magnitude) do
    calculate_largest_magnitude(tail, original, original, magnitude)
  end
  defp calculate_largest_magnitude([head1 | _tail1] = list1, [head2 | tail2], original, magnitude) do
    new_magnitude =
      head1
      |> sum_two(head2)
      |> calculate_magnitude()
      |> max(magnitude)

    calculate_largest_magnitude(list1, tail2, original, new_magnitude)
  end
end
