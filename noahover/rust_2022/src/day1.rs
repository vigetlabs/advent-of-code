type Elf = Vec<i32>;

#[aoc_generator(day1)]
pub fn input_generator(input: &str) -> Vec<Elf> {
    input
        .trim()
        .split("\n\n")
        .map(|group| {
            group
                .lines()
                .map(|line| {
                    line
                        .trim()
                        .parse()
                        .unwrap()
                })
                .collect()
        })
        .collect()
}

#[aoc(day1, part1)]
pub fn solve_part1(input: &[Elf]) -> i32 {
    input
        .iter()
        .map(|elf| {
            elf.iter().sum()
        })
        .reduce(|acc, sum| {
            if acc >= sum { acc } else { sum }
        })
        .unwrap()
}

#[aoc(day1, part2)]
pub fn solve_part2(input: &[Elf]) -> i32 {
    let (first, second, third) = input
        .iter()
        .map(|elf| {
            (elf.iter().sum(), 0, 0)
        })
        .reduce(|(first, second, third), (sum, _, _)| {
            if sum >= first {
                (sum, first, second)
            } else if sum >= second {
                (first, sum, second)
            } else if sum >= third {
                (first, second, sum)
            } else {
                (first, second, third)
            }
        })
        .unwrap();

    first + second + third
}
