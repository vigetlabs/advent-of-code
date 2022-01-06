defmodule Day17Test do
  use ExUnit.Case
  doctest Day17

  import Day17

  test "day 17 part 2 example - count initial velocities" do
    answer =
      File.read!("input/day17_example.txt")
      |> parse_input()
      |> count_initial_velocities()
    assert answer == 112
  end

  test "day 17 part 2 - count initial velocities" do
    answer =
      File.read!("input/day17.txt")
      |> parse_input()
      |> count_initial_velocities()
    assert answer == 1566
  end
end
