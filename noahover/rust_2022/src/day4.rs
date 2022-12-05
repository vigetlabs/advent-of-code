type Assignment = (i32, i32);
type Pair = (Assignment, Assignment);

#[aoc_generator(day4)]
pub fn input_generator(input: &str) -> Vec<Pair> {
    input
        .lines()
        .map(|line| {
            let mut assignments = line
                .split(",")
                .map(|elf| {
                    let mut sections = elf
                        .split("-")
                        .map(|section| {
                            section.parse().unwrap()
                        });

                    (
                        sections.next().unwrap(),
                        sections.next().unwrap(),
                    )
                });

            (
                assignments.next().unwrap(),
                assignments.next().unwrap(),
            )
        }).collect()
}

#[aoc(day4, part1)]
pub fn solve_part1(input: &[Pair]) -> usize {
    input
        .iter()
        .filter(|((s11, s12), (s21, s22))| {
            (s11 <= s21 && s12 >= s22) || (s21 <= s11 && s22 >= s12)
        }).count()
}

#[aoc(day4, part2)]
pub fn solve_part2(input: &[Pair]) -> usize {
    input
        .iter()
        .filter(|((s11, s12), (s21, s22))| {
            (s11 <= s21 && s12 >= s21) || (s21 <= s11 && s22 >= s11)
        }).count()
}
