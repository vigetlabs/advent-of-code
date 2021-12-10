#[aoc_generator(day10)]
pub fn input_generator(input: &str) -> Vec<String> {
    input.split("\n").map(|s| s.to_string()).collect()
}

#[aoc(day10, part1)]
pub fn part1(lines: &[String]) -> usize {
    lines.iter().map(score_line).sum()
}

#[aoc(day10, part2)]
pub fn part2(lines: &[String]) -> usize {
    0
}

fn score_line(line: &String) -> usize {
    let mut stack = Vec::with_capacity(line.len() / 2);
    let mut chars = line.chars();

    while let Some(char) = chars.next() {
        match char {
            '(' => stack.push('('),
            '[' => stack.push('['),
            '{' => stack.push('{'),
            '<' => stack.push('<'),

            ')' => match stack.pop() {
                Some('(') => (),
                Some(_) => return score_for(')'),
                None => return 0,
            },
            ']' => match stack.pop() {
                Some('[') => (),
                Some(_) => return score_for(']'),
                None => return 0,
            },
            '}' => match stack.pop() {
                Some('{') => (),
                Some(_) => return score_for('}'),
                None => return 0,
            },
            '>' => match stack.pop() {
                Some('<') => (),
                Some(_) => return score_for('>'),
                None => return 0,
            },
            _ => panic!("Unknown character {}", char),
        }
    }

    0
}

fn score_for(symbol: char) -> usize {
    match symbol {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => panic!("No score for {}", symbol),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
