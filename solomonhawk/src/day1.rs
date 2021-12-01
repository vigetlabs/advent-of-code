#[aoc_generator(day1)]
pub fn input_generator(input: &str) -> Vec<usize> {
    input
        .trim()
        .split("\n")
        .map(|n| n.parse().unwrap())
        .collect()
}

#[aoc(day1, part1)]
pub fn part1(nums: &[usize]) -> usize {
    for n in nums.iter() {
        for m in nums.iter() {
            if n + m == 2020 {
                return n * m;
            }
        }
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample1() {
        assert_eq!(part1(&[1000, 1020]), 1020000);
    }
}
