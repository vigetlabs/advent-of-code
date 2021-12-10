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
    let mut scores: Vec<_> = lines
        .iter()
        .filter_map(is_incomplete)
        .map(completion_string)
        .map(score_completion)
        .collect();

    scores.sort_unstable();
    scores[scores.len() / 2]
}

fn score_line(line: &String) -> usize {
    let mut stack = Vec::with_capacity(line.len() / 2);
    let mut chars = line.chars();

    while let Some(char) = chars.next() {
        match char {
            ')' => match stack.pop() {
                Some('(') => (),                  // match
                Some(_) => return score_for(')'), // corrupted
                None => return 0,                 // incomplete
            },
            ']' => match stack.pop() {
                Some('[') => (),                  // match
                Some(_) => return score_for(']'), // corrupted
                None => return 0,                 // incomplete
            },
            '}' => match stack.pop() {
                Some('{') => (),                  // match
                Some(_) => return score_for('}'), // corrupted
                None => return 0,                 // incomplete
            },
            '>' => match stack.pop() {
                Some('<') => (),                  // match
                Some(_) => return score_for('>'), // corrupted
                None => return 0,                 // incomplete
            },
            c => stack.push(c),
        }
    }

    0
}

fn is_incomplete(line: &String) -> Option<Vec<char>> {
    let mut stack = Vec::with_capacity(line.len() / 2);
    let mut chars = line.chars();

    while let Some(char) = chars.next() {
        match char {
            ')' => match stack.pop() {
                Some('(') => (),            // match
                Some(_) => return None,     // corrupted
                None => return Some(stack), // incomplete
            },
            ']' => match stack.pop() {
                Some('[') => (),            // match
                Some(_) => return None,     // corrupted
                None => return Some(stack), // incomplete
            },
            '}' => match stack.pop() {
                Some('{') => (),            // match
                Some(_) => return None,     // corrupted
                None => return Some(stack), // incomplete
            },
            '>' => match stack.pop() {
                Some('<') => (),            // match
                Some(_) => return None,     // corrupted
                None => return Some(stack), // incomplete
            },
            c => stack.push(c),
        }
    }

    Some(stack)
}

fn completion_string(chars: Vec<char>) -> String {
    let mut completion = String::from("");

    for c in chars.iter().rev() {
        match c {
            '(' => completion.push_str(")"),
            '[' => completion.push_str("]"),
            '{' => completion.push_str("}"),
            '<' => completion.push_str(">"),
            _ => panic!("Unknown completion string for {}", c),
        }
    }

    completion
}

fn score_completion(completion: String) -> usize {
    completion.chars().fold(0, |acc, c| match c {
        ')' => acc * 5 + 1,
        ']' => acc * 5 + 2,
        '}' => acc * 5 + 3,
        '>' => acc * 5 + 4,
        _ => panic!("Unknown completion string for {}", c),
    })
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
