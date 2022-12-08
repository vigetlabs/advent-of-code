type Trees = Vec<Vec<i32>>;

#[aoc_generator(day8)]
pub fn input_generator(input: &str) -> Trees {
    let mut trees = Vec::new();

    for line in input.lines() {
        let mut row = Vec::new();

        for height in line.chars() {
            let height: i32 = height.to_digit(10).unwrap().try_into().unwrap();

            row.push(height);
        }

        trees.push(row);
    }

    trees
}

#[aoc(day8, part1)]
pub fn solve_part1(input: &Trees) -> i32 {
    let side = input.len();

    let mut results = vec![vec![0; side]; side];

    for col in 0..side {
        let mut tallest = -1;

        for row in 0..side {
            if tallest == 9 {
                break;
            }

            let curr = input[row][col];

            if curr > tallest {
                results[row][col] = 1;
                tallest = curr;
            }
        }
    }

    for col in 0..side {
        let mut tallest = -1;

        for row in (0..side).rev() {
            if tallest == 9 {
                break;
            }

            let curr = input[row][col];

            if curr > tallest {
                results[row][col] = 1;
                tallest = curr;
            }
        }
    }

    for row in 0..side {
        let mut tallest = -1;

        for col in 0..side {
            if tallest == 9 {
                break;
            }

            let curr = input[row][col];

            if curr > tallest {
                results[row][col] = 1;
                tallest = curr;
            }
        }
    }

    for row in 0..side {
        let mut tallest = -1;

        for col in (0..side).rev() {
            if tallest == 9 {
                break;
            }

            let curr = input[row][col];

            if curr > tallest {
                results[row][col] = 1;
                tallest = curr;
            }
        }
    }

    results
        .iter()
        .map(|row| row.iter().sum::<i32>())
        .sum()
}

#[aoc(day8, part2)]
pub fn solve_part2(input: &Trees) -> i32 {
    let side = input.len();

    let mut best_score = 0;

    for (row, list) in input.iter().enumerate() {
        for (col, height) in list.iter().enumerate() {
            let mut up_score = 0;

            for row2 in (0..row).rev() {
                up_score += 1;

                let height2 = input[row2][col];

                if height2 >= *height {
                    break;
                }
            }

            let mut down_score = 0;

            for row2 in (row + 1)..side {
                down_score += 1;

                let height2 = input[row2][col];

                if height2 >= *height {
                    break;
                }
            }

            let mut left_score = 0;

            for col2 in (0..col).rev() {
                left_score += 1;

                let height2 = input[row][col2];

                if height2 >= *height {
                    break;
                }
            }

            let mut right_score = 0;

            for col2 in (col + 1)..side {
                right_score += 1;

                let height2 = input[row][col2];

                if height2 >= *height {
                    break;
                }
            }

            let score = up_score * down_score * left_score * right_score;

            if score > best_score {
                best_score = score;
            }
        }
    }

    best_score
}
