defmodule Day16 do
  @moduledoc """
  Advent of Code 2021 Day 16
  """

  def parse_input(input) do
    input
    |> String.trim_trailing("\n")
    |> String.graphemes()
    |> to_binary()
  end

  defp to_binary([]), do: ""
  defp to_binary([char | tail]) do
    four_bits = case char do
      "0" -> "0000"
      "1" -> "0001"
      "2" -> "0010"
      "3" -> "0011"
      "4" -> "0100"
      "5" -> "0101"
      "6" -> "0110"
      "7" -> "0111"
      "8" -> "1000"
      "9" -> "1001"
      "A" -> "1010"
      "B" -> "1011"
      "C" -> "1100"
      "D" -> "1101"
      "E" -> "1110"
      "F" -> "1111"
    end
    four_bits <> to_binary(tail)
  end

  @doc """
  Day 16 Part 1

  Find sum of the version numbers of all packets
  """
  def version_numbers(input) do
    len = String.length(input)
    if len < 11 do
      0
    else
      version =
        input
        |> String.slice(0, 3)
        |> String.to_integer(2)

      type_id =
        input
        |> String.slice(3, 3)
        |> String.to_integer(2)

      case type_id do
        4 ->
          remainder =
            input
            |> String.slice(6, len)
            |> finish_literal()

          version + version_numbers(remainder)

        _ ->
          {remainder, versions} =
            input
            |> String.slice(6, len)
            |> parse_operator()

          version + versions + version_numbers(remainder)
      end
    end
  end

  defp finish_literal(input) do
    len = String.length(input)
    case String.at(input, 0) do
      "1" ->
        input
        |> String.slice(5, len)
        |> finish_literal()

      "0" -> String.slice(input, 5, len)
    end
  end

  defp parse_operator(input) do
    len = String.length(input)
    case String.at(input, 0) do
      "0" ->
        length =
          input
          |> String.slice(1, 15)
          |> String.to_integer(2)

        versions =
          input
          |> String.slice(16, length)
          |> version_numbers()

        remainder = String.slice(input, 16 + length, len)

        {remainder, versions}

      "1" ->
        number =
          input
          |> String.slice(1, 11)
          |> String.to_integer(2)

        input
        |> String.slice(12, len)
        |> parse_subpackets(0, number, 0)
    end
  end

  defp parse_subpackets(input, max, max, versions), do: {input, versions}
  defp parse_subpackets(input, current, max, versions) do
    len = String.length(input)

    version =
      input
      |> String.slice(0, 3)
      |> String.to_integer(2)

    type_id =
      input
      |> String.slice(3, 3)
      |> String.to_integer(2)

    case type_id do
      4 ->
        remainder =
          input
          |> String.slice(6, len)
          |> finish_literal()

        parse_subpackets(remainder, current + 1, max, version + versions)

      _ ->
        {remainder, operator_versions} =
          input
          |> String.slice(6, len)
          |> parse_operator()

        parse_subpackets(remainder, current + 1, max, version + operator_versions + versions)
    end
  end

  @doc """
  Day 16 Part 2

  Find value of BITS transmission
  """
  def find_value(input) do
    case find_value_loop(input) do
      {_remainder, value} -> value
      :invalid -> 0
    end
  end

  defp find_value_loop(input) do
    len = String.length(input)
    if len < 11 do
      :invalid
    else
      type_id =
        input
        |> String.slice(3, 3)
        |> String.to_integer(2)

      remainder = String.slice(input, 6, len)

      case type_id do
        4 -> find_literal(remainder)
        _ -> solve_operator(remainder, type_id)
      end
    end
  end

  defp find_literal(input) do
    {remainder, value_str} = find_literal_loop(input, "")

    {remainder, String.to_integer(value_str, 2)}
  end

  defp find_literal_loop(input, value_str) do
    len = String.length(input)
    remainder = String.slice(input, 5, len)
    bits = String.slice(input, 1, 4)
    case String.at(input, 0) do
      "0" -> {remainder, value_str <> bits}
      "1" -> find_literal_loop(remainder, value_str <> bits)
    end
  end

  defp solve_operator(input, type_id) do
    len = String.length(input)
    {remainder, values} = case String.at(input, 0) do
      "0" ->
        length =
          input
          |> String.slice(1, 15)
          |> String.to_integer(2)

        remainder = String.slice(input, 16 + length, len)

        values =
          input
          |> String.slice(16, length)
          |> find_values_with_length()

        {remainder, values}

      "1" ->
        number =
          input
          |> String.slice(1, 11)
          |> String.to_integer(2)

        input
        |> String.slice(12, len)
        |> find_values_with_number(0, number, [])
    end

    {remainder, solve_problem(type_id, values)}
  end

  defp find_values_with_length(input) do
    case find_value_loop(input) do
      :invalid -> []
      {remainder, value} -> [value | find_values_with_length(remainder)]
    end
  end

  defp find_values_with_number(input, max, max, values), do: {input, values}
  defp find_values_with_number(input, current, max, values) do
    {remainder, value} = find_value_loop(input)
    find_values_with_number(remainder, current + 1, max, values ++ [value])
  end

  defp solve_problem(type_id, values) do
    case type_id do
      0 -> Enum.sum(values)
      1 -> Enum.product(values)
      2 -> Enum.min(values)
      3 -> Enum.max(values)
      5 ->
        [first, second] = values
        cond do
          first > second -> 1
          true -> 0
        end

      6 ->
        [first, second] = values
        cond do
          first < second -> 1
          true -> 0
        end

      7 ->
        [first, second] = values
        cond do
          first == second -> 1
          true -> 0
        end
    end
  end
end
