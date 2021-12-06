#[aoc_generator(day6)]
pub fn input_generator(input: &str) -> Vec<u8> {
    input.split(",").map(|n| n.parse().unwrap()).collect()
}

#[aoc(day6, part1)]
pub fn part1(fish: &Vec<u8>) -> usize {
    let mut fish = fish.clone();

    for _ in 0..80 {
        simulate(&mut fish);
    }

    fish.len()
}

// #[aoc(day2, part2)]
// pub fn part2(fish: &[u8]) -> usize {
//     0
// }

fn simulate(fish: &mut Vec<u8>) {
    let mut new_fish = 0;

    for f in fish.iter_mut() {
        match f {
            n if *n == 0 => {
                *n = 6;
                new_fish += 1;
            }
            n => *n -= 1,
        }
    }

    if new_fish > 0 {
        for _ in 0..new_fish {
            fish.push(8);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input() {
        let input = input_generator("1,2,3");

        assert_eq!(input[0], 1);
        assert_eq!(input[1], 2);
        assert_eq!(input[2], 3);
    }
}
