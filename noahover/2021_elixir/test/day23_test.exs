defmodule Day23Test do
  use ExUnit.Case

  test "day 23 part 1 example - lowest energy" do
    answer =
      File.read!("input/day23_example.txt")
      |> Day23Part1.parse_input()
      |> Day23Part1.lowest_energy()
    assert answer == 12521
  end

  test "day 23 part 1 - lowest energy" do
    answer =
      File.read!("input/day23.txt")
      |> Day23Part1.parse_input()
      |> Day23Part1.lowest_energy()
    assert answer == 11608
  end

  test "day 23 part 2 example - lowest energy" do
    answer =
      File.read!("input/day23_example.txt")
      |> Day23Part2.parse_input()
      |> Day23Part2.lowest_energy()
    assert answer == 44169
  end

  test "day 23 part 2 - lowest energy" do
    answer =
      File.read!("input/day23.txt")
      |> Day23Part2.parse_input()
      |> Day23Part2.lowest_energy()
    assert answer == 46754
  end
end
