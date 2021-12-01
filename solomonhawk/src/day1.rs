#[aoc_generator(day1)]
pub fn input_generator(input: &str) -> Vec<usize> {
    input.split("\n").map(|n| n.parse().unwrap()).collect()
}

#[aoc(day1, part1)]
pub fn part1(nums: &[usize]) -> usize {
    count_increasing_seqs(nums.iter().cloned())
}

#[aoc(day1, part2)]
pub fn part2(nums: &[usize]) -> usize {
    count_increasing_seqs(nums.windows(3).map(|ns| ns.iter().sum()))
}

pub fn count_increasing_seqs(nums: impl Iterator<Item = usize>) -> usize {
    nums.fold((0, 0), |(count, last), current| {
        if last > 0 && current > last {
            return (count + 1, current);
        }

        return (count, current);
    })
    .0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input() {
        assert_eq!(
            input_generator("199\n200\n208\n210\n200\n207\n240\n269\n260\n263"),
            [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
        );
    }

    #[test]
    fn increasing_seqs() {
        assert_eq!(
            count_increasing_seqs([199, 200, 208, 200].iter().cloned()),
            2
        );
    }

    #[test]
    fn sample1() {
        assert_eq!(
            part1(&[199, 200, 208, 210, 200, 207, 240, 269, 260, 263]),
            7
        );
    }

    #[test]
    fn sample2() {
        assert_eq!(
            part2(&[199, 200, 208, 210, 200, 207, 240, 269, 260, 263]),
            5
        );
    }
}
