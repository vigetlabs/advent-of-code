use std::error::Error;
use std::fmt;
use std::str::FromStr;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Bingo {
    picks: Vec<usize>,
    boards: Vec<Board>,
    winners: Vec<Board>,
    dimension: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Board {
    spaces: Vec<Space>,
    winner: bool,
    last_pick: Option<usize>,
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
        for pick in self.picks.clone() {
            self.mark_spaces(pick);
            self.check_winners(pick);
        }

        self
    }

    fn mark_spaces(&mut self, pick: usize) {
        for board in &mut self.boards {
            if board.winner {
                continue;
            }

            for space in &mut board.spaces {
                if space.value == pick {
                    space.picked = true;
                }
            }
        }
    }

    fn check_winners(&mut self, pick: usize) {
        for board in &mut self.boards {
            if board.winner {
                continue;
            }

            for i in 0..self.dimension {
                if check_row_win(board, i) || check_col_win(board, i) {
                    board.winner = true;
                    board.last_pick = Some(pick);

                    self.winners.push(board.clone());

                    break;
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct BoardParseError;

impl Error for BoardParseError {}

impl fmt::Display for BoardParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Board parse error")
    }
}

impl FromStr for Board {
    type Err = BoardParseError;

    fn from_str(board: &str) -> Result<Board, Self::Err> {
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
        Ok(Board {
            spaces,
            winner: false,
            last_pick: None,
        })
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
        .map(|board| board.parse().expect("Failed to parse board"))
        .collect();

    bingo
}

#[aoc(day4, part1)]
pub fn part1(bingo: &Bingo) -> usize {
    let mut bingo = bingo.clone();

    bingo.play();

    if let Some(winner) = &bingo.winners.first() {
        sum_unmarked_spaces(winner) * winner.last_pick.expect("Winning board missing last_pick")
    } else {
        println!("A winner was not found!");
        0
    }
}

#[aoc(day4, part2)]
pub fn part2(bingo: &Bingo) -> usize {
    let mut bingo = bingo.clone();

    bingo.play();

    if let Some(last_winner) = &bingo.winners.last() {
        sum_unmarked_spaces(last_winner)
            * last_winner
                .last_pick
                .expect("Winning board missing last_pick")
    } else {
        println!("A winner was not found!");
        0
    }
}

fn sum_unmarked_spaces(board: &Board) -> usize {
    board
        .spaces
        .iter()
        .filter(|s| !s.picked)
        .map(|s| s.value)
        .sum()
}

// TODO(shawk): I should write some tests...
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input() {
        let bingo = input_generator(
            "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

        22 13 17 11  0
         8  2 23  4 24
        21  9 14 16  7
         6 10  3 18  5
         1 12 20 15 19

         3 15  0  2 22
         9 18 13 17  5
        19  8  7 25 23
        20 11 10 24  4
        14 21 16 12  6

        14 21 17 24  4
        10 16 15  9 19
        18  8 23 26 20
        22 11 13  6  5
         2  0 12  3  7",
        );

        assert_eq!(
            bingo.picks,
            [
                7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8,
                19, 3, 26, 1
            ]
        );

        assert_eq!(bingo.boards.len(), 3);

        assert_eq!(bingo.boards[0].spaces.len(), 25);
        assert_eq!(bingo.boards[1].spaces.len(), 25);
        assert_eq!(bingo.boards[2].spaces.len(), 25);
    }
}
