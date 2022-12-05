type Stack = Vec<char>;
type Instruction = (usize, usize, usize);
type Input = (Vec<Stack>, Vec<Instruction>);

#[aoc_generator(day5)]
pub fn input_generator(input: &str) -> Input {
    let info: Vec<&str> = input
        .split("\n\n")
        .collect();

    (
        parse_stacks(info[0]),
        parse_instructions(info[1]),
    )
}

#[aoc(day5, part1)]
pub fn solve_part1(input: &Input) ->  String {
    let (mut stacks, instructions) = input.clone();

    for (amount, origin, destination) in instructions {
        let origin = origin - 1;
        let destination = destination - 1;

        for _ in 0..amount {
            let current_char = stacks[origin].pop().unwrap();

            stacks[destination].push(current_char);
        }
    }

    get_stack_tops(stacks)
}

#[aoc(day5, part2)]
pub fn solve_part2(input: &Input) ->  String {
    let (mut stacks, instructions) = input.clone();

    for (amount, origin, destination) in instructions {
        let origin = origin - 1;
        let destination = destination - 1;

        let mut temp = Vec::with_capacity(amount);

        for _ in 0..amount {
            let current_char = stacks[origin].pop().unwrap();

            temp.push(current_char);
        }

        temp.reverse();
        for current_char in temp {
            stacks[destination].push(current_char);
        }
    }

    get_stack_tops(stacks)
}

fn parse_stacks(input: &str) -> Vec<Stack> {
    let mut reverse_lines = input.rsplit("\n");
    let last_line = reverse_lines
        .next()
        .unwrap();

    let num_of_stacks = last_line
        .split_whitespace()
        .count();

    let mut stacks: Vec<Stack> = vec![Vec::new(); num_of_stacks];

    reverse_lines.for_each(|line| {
        let chars: Vec<char> = line
            .chars()
            .collect();

        let mut i = 1;
        for stack in stacks.iter_mut() {
            let index = (4 * i) - 3;

            if index < chars.len() {
                let current_char = chars[(4 * i) - 3];

                if current_char != ' ' {
                    stack.push(current_char);
                }
            }

            i = i + 1;
        }
    });

    stacks
}

fn parse_instructions(input: &str) -> Vec<Instruction> {
    input.lines()
        .map(|line| {
            let words: Vec<&str> = line
                .split_whitespace()
                .collect();

            (
                words[1].parse().unwrap(),
                words[3].parse().unwrap(),
                words[5].parse().unwrap(),
            )
        }).collect()
}

fn get_stack_tops(stacks: Vec<Stack>) -> String {
    let stack_tops: Vec<String> = stacks
        .iter()
        .map(|stack| {
            stack.last().unwrap().to_string()
        }).collect();

    stack_tops.join("")
}
