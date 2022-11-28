defmodule Day16Test do
  use ExUnit.Case
  doctest Day16

  import Day16

  test "day 16 part 1 example 1 - sum of version numbers" do
    answer =
      File.read!("input/day16_example1.txt")
      |> parse_input()
      |> version_numbers()
    assert answer == 16
  end

  test "day 16 part 1 example 2 - sum of version numbers" do
    answer =
      File.read!("input/day16_example2.txt")
      |> parse_input()
      |> version_numbers()
    assert answer == 12
  end

  test "day 16 part 1 example 3 - sum of version numbers" do
    answer =
      File.read!("input/day16_example3.txt")
      |> parse_input()
      |> version_numbers()
    assert answer == 23
  end

  test "day 16 part 1 example 4 - sum of version numbers" do
    answer =
      File.read!("input/day16_example4.txt")
      |> parse_input()
      |> version_numbers()
    assert answer == 31
  end

  test "day 16 part 1 - sum of version numbers" do
    answer =
      File.read!("input/day16.txt")
      |> parse_input()
      |> version_numbers()
    assert answer == 923
  end

  test "day 16 part 2 example 5 - find value of bits transmission" do
    answer =
      File.read!("input/day16_example5.txt")
      |> parse_input()
      |> find_value()
    assert answer == 3
  end

  test "day 16 part 2 example 6 - find value of bits transmission" do
    answer =
      File.read!("input/day16_example6.txt")
      |> parse_input()
      |> find_value()
    assert answer == 54
  end

  test "day 16 part 2 example 7 - find value of bits transmission" do
    answer =
      File.read!("input/day16_example7.txt")
      |> parse_input()
      |> find_value()
    assert answer == 7
  end

  test "day 16 part 2 example 8 - find value of bits transmission" do
    answer =
      File.read!("input/day16_example8.txt")
      |> parse_input()
      |> find_value()
    assert answer == 9
  end

  test "day 16 part 2 example 9 - find value of bits transmission" do
    answer =
      File.read!("input/day16_example9.txt")
      |> parse_input()
      |> find_value()
    assert answer == 1
  end

  test "day 16 part 2 example 10 - find value of bits transmission" do
    answer =
      File.read!("input/day16_example10.txt")
      |> parse_input()
      |> find_value()
    assert answer == 0
  end

  test "day 16 part 2 example 11 - find value of bits transmission" do
    answer =
      File.read!("input/day16_example11.txt")
      |> parse_input()
      |> find_value()
    assert answer == 0
  end

  test "day 16 part 2 example 12 - find value of bits transmission" do
    answer =
      File.read!("input/day16_example12.txt")
      |> parse_input()
      |> find_value()
    assert answer == 1
  end

  test "day 16 part 2 - find value of bits transmission" do
    answer =
      File.read!("input/day16.txt")
      |> parse_input()
      |> find_value()
    assert answer == 258888628940
  end
end
