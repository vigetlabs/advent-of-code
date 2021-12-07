#[aoc_generator(day7)]
pub fn input_generator(input: &str) -> Vec<u32> {
    input.split(",").map(|s| s.parse().unwrap()).collect()
}

#[aoc(day7, part1)]
pub fn part1(positions: &[u32]) -> u32 {
    let min = positions.iter().min().unwrap();
    let max = positions.iter().max().unwrap();

    (*min..*max)
        .map(|i| {
            positions
                .iter()
                .map(|p| if i > *p { i - p } else { p - i })
                .sum()
        })
        .min()
        .unwrap()
}

#[aoc(day7, part2)]
pub fn part2(positions: &[u32]) -> u32 {
    let min = positions.iter().min().unwrap();
    let max = positions.iter().max().unwrap();

    (*min..*max)
        .map(|i| {
            positions
                .iter()
                .map(|p| if i > *p { fuel(i - p) } else { fuel(p - i) })
                .sum()
        })
        .min()
        .unwrap()
}

fn fuel(d: u32) -> u32 {
    (1..d + 1).sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fuel_test() {
        assert_eq!(fuel(4), 10);
    }
}
