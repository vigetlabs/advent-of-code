type RpsRound = (String, String);

#[aoc_generator(day2)]
pub fn input_generator(input: &str) -> Vec<RpsRound> {
    input
        .lines()
        .map(|l| {
            let mut round = l.trim().split(' ');
            (
                String::from(round.next().unwrap()),
                String::from(round.next().unwrap()),
            )
        }).collect()
}

#[aoc(day2, part1)]
pub fn solve_part1(input: &[RpsRound]) -> i32 {
    input
        .iter()
        .map(|(opp, you)| {
            match (opp.as_str(), you.as_str()) {
                ("A", "X") => 4,
                ("A", "Y") => 8,
                ("A", "Z") => 3,
                ("B", "X") => 1,
                ("B", "Y") => 5,
                ("B", "Z") => 9,
                ("C", "X") => 7,
                ("C", "Y") => 2,
                ("C", "Z") => 6,
                _ => 0,
            }
        }).sum()
}

#[aoc(day2, part2)]
pub fn solve_part2(input: &[RpsRound]) -> i32 {
    input
        .iter()
        .map(|(opp, outcome)| {
            match (opp.as_str(), outcome.as_str()) {
                ("A", "X") => 3,
                ("A", "Y") => 4,
                ("A", "Z") => 8,
                ("B", "X") => 1,
                ("B", "Y") => 5,
                ("B", "Z") => 9,
                ("C", "X") => 2,
                ("C", "Y") => 6,
                ("C", "Z") => 7,
                _ => 0,
            }
        }).sum()
}
