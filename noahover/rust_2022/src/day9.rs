use std::collections::HashSet;

type Instruction = (String, i32);

#[aoc_generator(day9)]
pub fn input_generator(input: &str) -> Vec<Instruction> {
    input
        .lines()
        .map(|line| {
            let mut iter = line.split_whitespace();

            (
                iter.next().unwrap().to_string(),
                iter.next().unwrap().parse().unwrap(),
            )
        })
        .collect()
}

#[aoc(day9, part1)]
pub fn solve_part1(input: &[Instruction]) -> usize {
    let mut hx = 0;
    let mut hy = 0;
    let mut tx = 0;
    let mut ty = 0;

    let mut set = HashSet::new();

    for (direction, distance) in input {
        for _ in 0..*distance {
            match direction.as_str() {
                "R" => hx += 1,
                "L" => hx -= 1,
                "U" => hy += 1,
                "D" => hy -= 1,
                _ => (),
            }

            let x_diff = hx - tx;
            let y_diff = hy - ty;

            match (x_diff, y_diff) {
                (2, _) => {
                    tx = hx - 1;
                    ty = hy;
                },
                (-2, _) => {
                    tx = hx + 1;
                    ty = hy;
                },
                (_, 2) => {
                    tx = hx;
                    ty = hy - 1;
                },
                (_, -2) => {
                    tx = hx;
                    ty = hy + 1;
                },
                _ => (),
            }

            set.insert((tx, ty));
        }
    }

    set.len()
}

#[aoc(day9, part2)]
pub fn solve_part2(input: &[Instruction]) -> usize {
    let mut positions = vec![(0, 0); 10];

    let mut set = HashSet::new();

    for (direction, distance) in input {
        for _ in 0..*distance {
            match direction.as_str() {
                "R" => positions[0].0 += 1,
                "L" => positions[0].0 -= 1,
                "U" => positions[0].1 += 1,
                "D" => positions[0].1 -= 1,
                _ => (),
            }

            for i in 0..9 {
                let j = i + 1;

                let x_diff = positions[i].0 - positions[j].0;
                let y_diff = positions[i].1 - positions[j].1;

                match (x_diff, y_diff) {
                    (2, 2) => {
                        positions[j].0 = positions[i].0 - 1;
                        positions[j].1 = positions[i].1 - 1;
                    },
                    (2, -2) => {
                        positions[j].0 = positions[i].0 - 1;
                        positions[j].1 = positions[i].1 + 1;
                    },
                    (-2, 2) => {
                        positions[j].0 = positions[i].0 + 1;
                        positions[j].1 = positions[i].1 - 1;
                    },
                    (-2, -2) => {
                        positions[j].0 = positions[i].0 + 1;
                        positions[j].1 = positions[i].1 + 1;
                    },
                    (2, _) => {
                        positions[j].0 = positions[i].0 - 1;
                        positions[j].1 = positions[i].1;
                    },
                    (-2, _) => {
                        positions[j].0 = positions[i].0 + 1;
                        positions[j].1 = positions[i].1;
                    },
                    (_, 2) => {
                        positions[j].0 = positions[i].0;
                        positions[j].1 = positions[i].1 - 1;
                    },
                    (_, -2) => {
                        positions[j].0 = positions[i].0;
                        positions[j].1 = positions[i].1 + 1;
                    },
                    _ => (),
                }
            }

            set.insert((positions[9].0, positions[9].1));
        }
    }

    set.len()
}
