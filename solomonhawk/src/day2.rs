use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Debug)]
pub enum ParseError {
    InvalidCmd,
    Parse(ParseIntError),
}

impl From<ParseIntError> for ParseError {
    fn from(err: ParseIntError) -> ParseError {
        ParseError::Parse(err)
    }
}

#[derive(Debug, PartialEq)]
pub enum Cmd {
    Up(usize),
    Down(usize),
    Forward(usize),
}

impl FromStr for Cmd {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Cmd, ParseError> {
        let mut iter = s.split(" ");
        let dir = iter.next().unwrap();
        let amount = iter.next().unwrap().parse()?;

        match dir {
            "up" => Ok(Cmd::Up(amount)),
            "down" => Ok(Cmd::Down(amount)),
            "forward" => Ok(Cmd::Forward(amount)),
            _ => Err(ParseError::InvalidCmd),
        }
    }
}

#[aoc_generator(day2)]
pub fn input_generator(input: &str) -> Vec<Cmd> {
    input
        .split("\n")
        .map(|n| Cmd::from_str(n).unwrap())
        .collect()
}

#[aoc(day2, part1)]
pub fn part1(commands: &[Cmd]) -> usize {
    let mut h: usize = 0;
    let mut d: usize = 0;

    for c in commands {
        match c {
            Cmd::Up(x) => d -= x,
            Cmd::Down(x) => d += x,
            Cmd::Forward(x) => h += x,
        }
    }

    h * d
}

#[aoc(day2, part2)]
pub fn part2(commands: &[Cmd]) -> usize {
    let mut h: usize = 0;
    let mut d: usize = 0;
    let mut aim: usize = 0;

    for c in commands {
        match c {
            Cmd::Up(x) => aim -= x,
            Cmd::Down(x) => aim += x,
            Cmd::Forward(x) => {
                h += x;
                d += aim * x;
            }
        }
    }

    h * d
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input() {
        let input = input_generator("up 3\ndown 5\nforward 1");

        assert_eq!(input[0], Cmd::Up(3));
        assert_eq!(input[1], Cmd::Down(5));
        assert_eq!(input[2], Cmd::Forward(1));
    }

    #[test]
    fn sample1() {
        let commands = vec![
            Cmd::Forward(5),
            Cmd::Down(5),
            Cmd::Forward(8),
            Cmd::Up(3),
            Cmd::Down(8),
            Cmd::Forward(2),
        ];

        assert_eq!(part1(&commands), 150);
    }

    #[test]
    fn sample2() {
        let commands = vec![
            Cmd::Forward(5),
            Cmd::Down(5),
            Cmd::Forward(8),
            Cmd::Up(3),
            Cmd::Down(8),
            Cmd::Forward(2),
        ];

        assert_eq!(part2(&commands), 900);
    }
}
