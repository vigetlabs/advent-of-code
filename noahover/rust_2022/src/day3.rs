use std::collections::HashSet;

type Rucksack = Vec<char>;

#[aoc_generator(day3)]
pub fn input_generator(input: &str) -> Vec<Rucksack> {
    input
        .lines()
        .map(|items| {
            items
                .chars()
                .collect()
        }).collect()
}

#[aoc(day3, part1)]
pub fn solve_part1(input: &[Rucksack]) -> i32 {
    input
        .iter()
        .map(|rucksack| {
            let length = rucksack.len();
            let half = length / 2;

            let first_compartment = vec_to_hashset(&rucksack[..half]);
            let second_compartment = vec_to_hashset(&rucksack[half..]);

            shared_item_value(first_compartment, second_compartment)
        }).sum()
}

#[aoc(day3, part2)]
pub fn solve_part2(input: &[Rucksack]) -> i32 {
    input
        .chunks(3)
        .map(|group| {
            let first_elf = vec_to_hashset(&group[0]);
            let second_elf = vec_to_hashset(&group[1]);
            let third_elf = vec_to_hashset(&group[2]);

            let intersection: HashSet<char> = first_elf
                .intersection(&second_elf)
                .map(|char_ptr| {
                    *char_ptr
                }).collect();

            shared_item_value(intersection, third_elf)
        }).sum()
}

fn vec_to_hashset(data: &[char]) -> HashSet<char> {
    HashSet::from_iter(data.iter().cloned())
}

fn shared_item_value(first: HashSet<char>, second: HashSet<char>) -> i32 {
    let shared_item_ptr = first
        .intersection(&second)
        .next()
        .unwrap();

    let shared_item = *shared_item_ptr as i32;

    if shared_item >= 97 {
        shared_item - 96
    } else {
        shared_item - 38
    }
}
