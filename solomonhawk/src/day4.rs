use std::fmt;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Bingo {
    pick_index: usize,
    picks: Vec<usize>,
    boards: Vec<Board>,
    winner: Option<Board>,
    dimension: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Board {
    spaces: Vec<Space>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Space {
    value: usize,
    picked: bool,
    row: usize,
    col: usize,
}

impl Bingo {
    fn play(&mut self) -> &mut Bingo {
        while self.winner.is_none() {
            if self.pick_index == self.picks.len() {
                println!("Ran out of picks!");
                break;
            }

            let pick = self.picks[self.pick_index];

            for board in &mut self.boards {
                mark_space(board, pick);
                // println!("{}\n", format!("{}", board));
                // println!("------------");
            }

            self.check_winner();

            if self.winner.is_none() {
                self.pick_index += 1;
            }
        }

        self
    }

    fn check_winner(&mut self) {
        if let Some(_) = &self.winner {
            return;
        }

        for board in &self.boards {
            for i in 0..self.dimension {
                if check_row_win(board, i) || check_col_win(board, i) {
                    self.winner = Some(board.clone());
                    break;
                }
            }
        }
    }
}

// ugly
impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut space_iter = self.spaces.iter();
        let mut display_str = String::from("");

        for _ in 0..5 {
            let (s1, s2, s3, s4, s5) = (
                space_iter.next().unwrap(),
                space_iter.next().unwrap(),
                space_iter.next().unwrap(),
                space_iter.next().unwrap(),
                space_iter.next().unwrap(),
            );

            display_str.push_str(&format!(
                "{} {} {} {} {}\n",
                if s1.picked { "x" } else { "·" },
                if s2.picked { "x" } else { "·" },
                if s3.picked { "x" } else { "·" },
                if s4.picked { "x" } else { "·" },
                if s5.picked { "x" } else { "·" }
            ));
        }

        write!(f, "{}", display_str)
    }
}

fn mark_space(board: &mut Board, pick: usize) {
    for space in &mut board.spaces {
        if space.value == pick {
            space.picked = true;
        }
    }
}

fn check_row_win(board: &Board, index: usize) -> bool {
    for space in board.spaces.iter().filter(|s| s.row == index) {
        if !space.picked {
            return false;
        }
    }

    true
}

fn check_col_win(board: &Board, index: usize) -> bool {
    for space in board.spaces.iter().filter(|s| s.col == index) {
        if !space.picked {
            return false;
        }
    }

    true
}

#[aoc_generator(day4)]
pub fn input_generator(input: &str) -> Bingo {
    let mut bingo = Bingo {
        dimension: 5,
        ..Bingo::default()
    };

    let (picks, boards) = input
        .split_once("\n\n")
        .expect("Could not split picks from boards");

    bingo.picks = picks
        .split(",")
        .map(|n| n.parse().expect("Failed to parse pick"))
        .collect();

    bingo.boards = boards
        .split("\n\n")
        .map(|board| {
            let spaces = board
                .split("\n")
                .enumerate()
                .flat_map(|(y, row)| {
                    row.split_whitespace()
                        .enumerate()
                        .map(move |(x, space)| Space {
                            value: space.parse().unwrap(),
                            picked: false,
                            row: y,
                            col: x,
                        })
                })
                .collect();
            Board { spaces }
        })
        .collect();

    bingo
}

#[aoc(day4, part1)]
pub fn part1(bingo: &Bingo) -> usize {
    let mut bingo = bingo.clone();

    bingo.play();

    if let Some(winner) = &bingo.winner {
        sum_umarked_spaces(winner) * bingo.picks[bingo.pick_index]
    } else {
        println!("A winner was not found!");
        0
    }
}

// #[aoc(day4, part2)]
// pub fn part2(bingo: &Bingo) -> usize {
//     0
// }

fn sum_umarked_spaces(board: &Board) -> usize {
    board
        .spaces
        .iter()
        .filter(|s| !s.picked)
        .map(|s| s.value)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn input() {
    //     assert_eq!(
    //         input_generator("199\n200\n208\n210\n200\n207\n240\n269\n260\n263"),
    //         []
    //     );
    // }
}
