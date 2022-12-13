#[aoc_generator(day11)]
pub fn input_generator(input: &str) -> (Vec<Vec<usize>>, Vec<String>, Vec<usize>, Vec<usize>, Vec<usize>, Vec<usize>) {
    let items = input
        .split("\n\n")
        .map(|monkey| {
            let lines: Vec<&str> = monkey.lines().collect();

            lines[1]
                .trim()
                .strip_prefix("Starting items: ")
                .unwrap()
                .split(", ")
                .map(|item| item.parse().unwrap())
                .collect()
        })
        .collect();

    let operation_types = input
        .split("\n\n")
        .map(|monkey| {
            let lines: Vec<&str> = monkey.lines().collect();

            let mut operation_info = lines[2]
                .trim()
                .strip_prefix("Operation: new = old ")
                .unwrap()
                .split_whitespace();

            let operation_info = (
                operation_info.next().unwrap(),
                operation_info.next().unwrap(),
            );

            let operation_info = match operation_info {
                ("*", "old") => ("^", 2),
                (t, v) => (t, v.parse().unwrap()),
            };

            operation_info.0.to_string()
        })
        .collect();

    let operation_values = input
        .split("\n\n")
        .map(|monkey| {
            let lines: Vec<&str> = monkey.lines().collect();

            let mut operation_info = lines[2]
                .trim()
                .strip_prefix("Operation: new = old ")
                .unwrap()
                .split_whitespace();

            let operation_info = (
                operation_info.next().unwrap(),
                operation_info.next().unwrap(),
            );

            let operation_info = match operation_info {
                ("*", "old") => ("^", 2),
                (t, v) => (t, v.parse().unwrap()),
            };

            operation_info.1
        })
        .collect();

    let tests = input
        .split("\n\n")
        .map(|monkey| {
            let lines: Vec<&str> = monkey.lines().collect();

            lines[3]
                .trim()
                .strip_prefix("Test: divisible by ")
                .unwrap()
                .parse()
                .unwrap()
        })
        .collect();

    let true_monkeys = input
        .split("\n\n")
        .map(|monkey| {
            let lines: Vec<&str> = monkey.lines().collect();

            lines[4]
                .trim()
                .strip_prefix("If true: throw to monkey ")
                .unwrap()
                .parse()
                .unwrap()
        })
        .collect();

    let false_monkeys = input
        .split("\n\n")
        .map(|monkey| {
            let lines: Vec<&str> = monkey.lines().collect();

            lines[5]
                .trim()
                .strip_prefix("If false: throw to monkey ")
                .unwrap()
                .parse()
                .unwrap()
        })
        .collect();

    (
        items,
        operation_types,
        operation_values,
        tests,
        true_monkeys,
        false_monkeys,
    )
}

#[aoc(day11, part1)]
pub fn solve_part1(input: &(Vec<Vec<usize>>, Vec<String>, Vec<usize>, Vec<usize>, Vec<usize>, Vec<usize>)) -> usize {
    let mut items: Vec<Vec<usize>> = vec![Vec::new(); input.0.len()];

    for (i, items_vec) in input.0.iter().enumerate() {
        for item in items_vec {
            items[i].push(*item);
        }
    }

    let operation_types = &input.1;
    let operation_values = &input.2;
    let tests = &input.3;
    let true_monkeys = &input.4;
    let false_monkeys = &input.5;

    let mut inspections = vec![0; items.len()];

    for _ in 0..20 {
        for i in 0..items.len() {
            inspections[i] += items[i].len();

            for j in 0..items[i].len() {
                let item = items[i][j];

                let new_item = match (operation_types[i].as_str(), operation_values[i]) {
                    ("+", v) => item + v,
                    ("*", v) => item * v,
                    ("^", _) => item * item,
                    _ => item,
                };

                let new_item = new_item / 3;

                if new_item % tests[i] == 0 {
                    items[true_monkeys[i]].push(new_item);
                } else {
                    items[false_monkeys[i]].push(new_item);
                }
            }

            items[i].clear();
        }
    }

    inspections.sort();
    inspections.reverse();

    inspections[0] * inspections[1]
}

#[aoc(day11, part2)]
pub fn solve_part2(input: &(Vec<Vec<usize>>, Vec<String>, Vec<usize>, Vec<usize>, Vec<usize>, Vec<usize>)) -> usize {
    let mut items: Vec<Vec<usize>> = vec![Vec::new(); input.0.len()];

    for (i, items_vec) in input.0.iter().enumerate() {
        for item in items_vec {
            items[i].push(*item);
        }
    }

    let operation_types = &input.1;
    let operation_values = &input.2;
    let tests = &input.3;
    let true_monkeys = &input.4;
    let false_monkeys = &input.5;

    let divisor: usize = tests
        .iter()
        .product();

    let mut inspections = vec![0; items.len()];

    for _ in 0..10_000 {
        for i in 0..items.len() {
            inspections[i] += items[i].len();

            for j in 0..items[i].len() {
                let item = items[i][j];

                let new_item = match (operation_types[i].as_str(), operation_values[i]) {
                    ("+", v) => item + v,
                    ("*", v) => item * v,
                    ("^", _) => item * item,
                    _ => item,
                };

                let new_item = new_item % divisor;

                if new_item % tests[i] == 0 {
                    items[true_monkeys[i]].push(new_item);
                } else {
                    items[false_monkeys[i]].push(new_item);
                }
            }

            items[i].clear();
        }
    }

    inspections.sort();
    inspections.reverse();

    inspections[0] * inspections[1]
}
