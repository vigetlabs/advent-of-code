defmodule Day13Test do
  use ExUnit.Case
  doctest Day13

  import Day13

  test "day 13 part 1 example - count dots after first fold" do
    answer =
      File.read!("input/day13_example.txt")
      |> parse_input()
      |> count_first_fold_dots()
    assert answer == 17
  end

  test "day 13 part 1 - count dots after first fold" do
    answer =
      File.read!("input/day13.txt")
      |> parse_input()
      |> count_first_fold_dots()
    assert answer == 687
  end

  test "day 13 part 2 example - get code from folding" do
    answer =
      File.read!("input/day13_example.txt")
      |> parse_input()
      |> folded_code()
    assert answer == """
                     #####...................................
                     #...#...................................
                     #...#...................................
                     #...#...................................
                     #####...................................
                     ........................................
                     """
  end

  test "day 13 part 2 - get code from folding" do
    answer =
      File.read!("input/day13.txt")
      |> parse_input()
      |> folded_code()
    # FGKCKBZG
    assert answer == """
                     ####..##..#..#..##..#..#.###..####..##..
                     #....#..#.#.#..#..#.#.#..#..#....#.#..#.
                     ###..#....##...#....##...###....#..#....
                     #....#.##.#.#..#....#.#..#..#..#...#.##.
                     #....#..#.#.#..#..#.#.#..#..#.#....#..#.
                     #.....###.#..#..##..#..#.###..####..###.
                     """
  end
end
