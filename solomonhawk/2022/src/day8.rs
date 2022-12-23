/*
--- Day 8: Treetop Tree House ---
The expedition comes across a peculiar patch of tall trees all planted carefully in a grid. The Elves explain that a previous expedition planted these trees as a reforestation effort. Now, they're curious if this would be a good location for a tree house.

First, determine whether there is enough tree cover here to keep a tree house hidden. To do this, you need to count the number of trees that are visible from outside the grid when looking directly along a row or column.

The Elves have already launched a quadcopter to generate a map with the height of each tree (your puzzle input). For example:

30373
25512
65332
33549
35390
Each tree is represented as a single digit whose value is its height, where 0 is the shortest and 9 is the tallest.

A tree is visible if all of the other trees between it and an edge of the grid are shorter than it. Only consider trees in the same row or column; that is, only look up, down, left, or right from any given tree.

All of the trees around the edge of the grid are visible - since they are already on the edge, there are no trees to block the view. In this example, that only leaves the interior nine trees to consider:

The top-left 5 is visible from the left and top. (It isn't visible from the right or bottom since other trees of height 5 are in the way.)
The top-middle 5 is visible from the top and right.
The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height 0 between it and an edge.
The left-middle 5 is visible, but only from the right.
The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge.
The right-middle 3 is visible from the right.
In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
With 16 trees visible on the edge and another 5 visible in the interior, a total of 21 trees are visible in this arrangement.

Consider your map; how many trees are visible from outside the grid?

--- Part Two ---
Content with the amount of tree cover available, the Elves just need to know the best spot to build their tree house: they would like to be able to see a lot of trees.

To measure the viewing distance from a given tree, look up, down, left, and right from that tree; stop if you reach an edge or at the first tree that is the same height or taller than the tree under consideration. (If a tree is right on the edge, at least one of its viewing distances will be zero.)

The Elves don't care about distant trees taller than those found by the rules above; the proposed tree house has large eaves to keep it dry, so they wouldn't be able to see higher than the tree house anyway.

In the example above, consider the middle 5 in the second row:

30373
25512
65332
33549
35390
Looking up, its view is not blocked; it can see 1 tree (of height 3).
Looking left, its view is blocked immediately; it can see only 1 tree (of height 5, right next to it).
Looking right, its view is not blocked; it can see 2 trees.
Looking down, its view is blocked eventually; it can see 2 trees (one of height 3, then the tree of height 5 that blocks its view).
A tree's scenic score is found by multiplying together its viewing distance in each of the four directions. For this tree, this is 4 (found by multiplying 1 * 1 * 2 * 2).

However, you can do even better: consider the tree of height 5 in the middle of the fourth row:

30373
25512
65332
33549
35390
Looking up, its view is blocked at 2 trees (by another tree with a height of 5).
Looking left, its view is not blocked; it can see 2 trees.
Looking down, its view is also not blocked; it can see 1 tree.
Looking right, its view is blocked at 2 trees (by a massive tree of height 9).
This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for the tree house.

Consider each tree on your map. What is the highest scenic score possible for any tree?
*/

const W: u8 = 4;
const S: u8 = 8;
const E: u8 = 12;
const N: u8 = 16;

type Packed = usize;

pub struct Patch {
    width: usize,
    height: usize,
    data: Vec<Vec<Packed>>,
}

#[aoc_generator(day8)]
pub fn input_generator(input: &str) -> Patch {
    let mut data: Vec<Vec<Packed>> = input
        .lines()
        .map(|l| l.as_bytes().iter().map(|b| (b - 48) as usize).collect())
        .collect();

    let width = data[0].len();
    let height = data.len();

    for y in 0..height {
        let mut max_x = 0;

        // forwards x pass for this row
        for x in 0..width {
            if x > 0 {
                max_x = max_x.max(v(data[y][x - 1]));
            }

            data[y][x] |= max_x << W;
        }

        max_x = 0;

        // backwards x pass for this row
        for xx in 0..width {
            let x = width - xx - 1;

            if x + 1 < width {
                max_x = max_x.max(v(data[y][x + 1]));
            }

            data[y][x] |= max_x << E;
        }
    }

    for x in 0..width {
        let mut max_y = 0;

        // forwards y pass for this column
        for y in 0..height {
            if y > 0 {
                max_y = max_y.max(v(data[y - 1][x]));
            }

            data[y][x] |= max_y << N;
        }

        max_y = 0;

        // backwards y pass for this column
        for yy in 0..height {
            let y = height - yy - 1;

            if y + 1 < height {
                max_y = max_y.max(v(data[y + 1][x]));
            }

            data[y][x] |= max_y << S;
        }
    }

    Patch {
        width,
        height,
        data,
    }
}

fn v(n: Packed) -> usize {
    n & 0b1111
}

fn w(n: Packed) -> usize {
    n >> W & 0b1111
}

fn s(n: Packed) -> usize {
    n >> S & 0b1111
}

fn e(n: Packed) -> usize {
    n >> E & 0b1111
}

fn n(n: Packed) -> usize {
    n >> N & 0b1111
}

fn is_visible(x: usize, y: usize, width: usize, height: usize, t: Packed) -> bool {
    let val = v(t);
    let on_edge = x == 0 || y == 0 || x == width - 1 || y == height - 1;
    on_edge || val > e(t) || val > w(t) || val > n(t) || val > s(t)
}

#[aoc(day8, part1)]
pub fn part1(patch: &Patch) -> usize {
    patch
        .data
        .iter()
        .enumerate()
        .flat_map(|(y, r)| {
            r.iter()
                .enumerate()
                .filter(move |&(x, t)| is_visible(x, y, patch.width, patch.height, *t))
        })
        .count()
}

#[aoc(day8, part2)]
pub fn part2(patch: &Patch) -> usize {
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input() {
        let input = "30373
25512
65332
33549
35390";
        assert!(matches!(
            input_generator(input),
            Patch {
                width: 5,
                height: 5,
                data: _
            }
        ));
    }

    #[test]
    fn sample1() {
        let input = "30373
25512
65332
33549
35390";

        assert_eq!(part1(&input_generator(input)), 21);
    }

    #[test]
    fn sample2() {
        let input = "30373
25512
65332
33549
35390";

        assert_eq!(part2(&input_generator(input)), 0);
    }
}
