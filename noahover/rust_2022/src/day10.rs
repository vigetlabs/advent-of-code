#[aoc_generator(day10)]
pub fn input_generator(input: &str) -> Vec<i32> {
    let mut rv = Vec::new();
    let mut x = 1;

    rv.push(1);

    for line in input.lines() {
        let mut words = line.split_whitespace();

        let command = words.next().unwrap();
        let amount: i32 = match words.next() {
            Some(s) => s.to_string().parse::<i32>().unwrap(),
            None => 0
        };

        match (command, amount) {
            ("noop", _) => rv.push(x),
            ("addx", i) => {
                rv.push(x);
                x += i;
                rv.push(x);
            },
            _ => (),
        }
    }

    rv
}

#[aoc(day10, part1)]
pub fn solve_part1(input: &[i32]) -> i32 {
    let mut cycle = 20;
    let mut cycle_index = 19;
    let mut rv = 0;

    while cycle_index < input.len() {
        let x = input[cycle_index];

        rv += x * cycle;
        cycle += 40;
        cycle_index += 40;
    }

    rv
}

#[aoc(day10, part2)]
pub fn solve_part2(input: &[i32]) -> usize {
    for (i, x) in input.iter().enumerate() {
        let position: i32 = (i % 40).try_into().unwrap();

        let printed = if position >= x - 1 && position <= x + 1 {
            "#"
        } else {
            "."
        };

        if position + 1 == 40 {
            println!("{}", printed);
        } else {
            print!("{}", printed);
        }
    }

    0
}
