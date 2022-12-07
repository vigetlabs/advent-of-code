use std::collections::HashMap;

type Directories = HashMap<(String, usize), i32>;

#[aoc_generator(day7)]
pub fn input_generator(input: &str) -> Directories {
    let mut directories = HashMap::new();
    directories.insert((String::from("/"), 0), 0);
    let mut current_dirs = vec![(String::from("/"), 0)];
    let mut visited_dirs: Vec<String> = Vec::new();
    let mut created_dirs: Vec<String> = Vec::new();

    for line in input.lines() {
        let mut iter = line.split_whitespace();

        let first = iter.next().unwrap();
        let second = iter.next().unwrap();
        let third = match iter.next() {
            Some(s) => s,
            None => ""
        };

        match (first, second, third) {
            ("$", "cd", "/") => current_dirs = vec![(String::from("/"), 0)],
            ("$", "cd", "..") => {
                current_dirs.pop();
            },
            ("$", "cd", dir_name) => {
                let visited_count = visited_dirs
                    .iter()
                    .filter(|dir| dir == &&dir_name.to_string())
                    .count();

                current_dirs.push((dir_name.to_string(), visited_count));
                visited_dirs.push(dir_name.to_string());
            },
            ("$", "ls", "") => (), // do nothing
            ("dir", dir_name, "") => {
                let created_count = created_dirs
                    .iter()
                    .filter(|dir| dir == &&dir_name.to_string())
                    .count();

                directories.insert((dir_name.to_string(), created_count), 0);
                created_dirs.push(dir_name.to_string());
            },
            (file_size, _, "") => {
                for dir in current_dirs.clone() {
                    let curr_value = directories.get(&dir).unwrap();
                    let file_size: i32 = file_size.to_string().parse().unwrap();
                    let new_value = curr_value + file_size;

                    directories.insert(dir, new_value);
                }
            }
            _ => (), // invalid input
        };
    }

    directories
}

#[aoc(day7, part1)]
pub fn solve_part1(input: &Directories) -> i32 {
    let max: i32 = 100_000;

    input.values()
        .filter(|v| **v <= max)
        .sum()
}

#[aoc(day7, part2)]
pub fn solve_part2(input: &Directories) -> i32 {
    let total: i32 = 70_000_000;
    let needed: i32 = 30_000_000;

    let used: i32 = *input
        .get(&(String::from("/"), 0))
        .unwrap();

    let min: i32 = used - (total - needed);

    *input.values()
        .filter(|v| **v >= min)
        .min()
        .unwrap()
}
