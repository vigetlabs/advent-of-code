defmodule Day4Test do
  use ExUnit.Case
  doctest Day4

  import Day4

  test "day 4 part 1 example - bingo winner" do
    {numbers, cards} =
      File.read!("input/day4_example.txt")
      |> parse_input()
    answer = bingo_winner(numbers, cards)
    assert answer == 4512
  end

  test "day 4 part 1 - bingo winner" do
    {numbers, cards} =
      File.read!("input/day4.txt")
      |> parse_input()
    answer = bingo_winner(numbers, cards)
    assert answer == 49686
  end

  test "day 4 part 2 example - final bingo winner" do
    {numbers, cards} =
      File.read!("input/day4_example.txt")
      |> parse_input()
    answer = final_bingo_winner(numbers, cards)
    assert answer == 1924
  end

  test "day 4 part 2 - final bingo winner" do
    {numbers, cards} =
      File.read!("input/day4.txt")
      |> parse_input()
    answer = final_bingo_winner(numbers, cards)
    assert answer == 26878
  end
end
