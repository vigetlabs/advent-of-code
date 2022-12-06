use std::collections::HashSet;

#[aoc_generator(day6)]
pub fn input_generator(input: &str) -> Vec<char> {
    input
        .chars()
        .collect()
}

#[aoc(day6, part1)]
pub fn solve_part1(input: &[char]) ->  i32 {
    let mut i = 0;

    let mut first = '0';
    let mut second = '0';
    let mut third = '0';
    let mut fourth = '0';

    for char in input {
        first = second;
        second = third;
        third = fourth;
        fourth = *char;
        i = i + 1;

        if i > 3
            && first != second
            && first != third
            && first != fourth
            && second != third
            && second != fourth
            && third != fourth {
            return i;
        }
    }

    i
}

#[aoc(day6, part2)]
pub fn solve_part2(input: &[char]) ->  i32 {
    let mut i = 0;

    let mut v: Vec<char> = Vec::with_capacity(14);

    for char in input {
        if v.len() == 14 {
            v.remove(0);
        }

        v.push(*char);
        i = i + 1;

        let h = vec_to_hashset(&v);

        if h.len() == 14 {
            return i;
        }
    }

    i
}

fn vec_to_hashset(data: &[char]) -> HashSet<char> {
    HashSet::from_iter(data.iter().cloned())
}
