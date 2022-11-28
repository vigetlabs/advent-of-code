defmodule Day15Test do
  use ExUnit.Case
  doctest Day15

  import Day15

  test "day 15 part 1 example - risk level of safest path" do
    answer =
      File.read!("input/day15_example.txt")
      |> parse_input()
      |> safest_path()
    assert answer == 40
  end

  test "day 15 part 1 - risk level of safest path" do
    answer =
      File.read!("input/day15.txt")
      |> parse_input()
      |> safest_path()
    assert answer == 613
  end

  test "day 15 part 2 example - risk level of safest extended path" do
    answer =
      File.read!("input/day15_example.txt")
      |> parse_input()
      |> extended_safest_path()
    assert answer == 315
  end

  test "day 15 part 2 - risk level of safest extended path" do
    answer =
      File.read!("input/day15.txt")
      |> parse_input()
      |> extended_safest_path()
    assert answer == 2899
  end
end
