const X: i8 = 100;
const Y: i8 = 100;

type HeightMap = [[i8; X as usize]; Y as usize];
type Point = (i8, i8);

// x, y
const OFFSETS: [Point; 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

#[aoc_generator(day9)]
pub fn input_generator(input: &str) -> HeightMap {
    let mut result = [[0; X as usize]; Y as usize];

    for (y, line) in input.split("\n").enumerate() {
        for (x, n) in line.bytes().enumerate() {
            result[y][x] = (n - 48) as i8;
        }
    }

    result
}

#[aoc(day9, part1)]
pub fn part1(map: &HeightMap) -> isize {
    points_iter()
        .filter(|point| is_local_minimum(point, &map))
        .map(|point| risk_level(&point, &map))
        .sum()
}

#[aoc(day9, part2)]
pub fn part2(map: &HeightMap) -> u64 {
    0
}

fn points_iter() -> impl Iterator<Item = Point> {
    (0..Y).flat_map(|y| (0..X).map(move |x| (x, y)))
}

fn is_local_minimum(point: &Point, map: &HeightMap) -> bool {
    let (x, y) = point;
    let height = map[*y as usize][*x as usize];

    for (a, b) in OFFSETS.iter() {
        let (xx, yy) = (x + a, y + b);

        if valid_point(xx, yy) {
            if map[yy as usize][xx as usize] <= height {
                return false;
            }
        }
    }

    true
}

fn risk_level(point: &Point, map: &HeightMap) -> isize {
    (map[point.1 as usize][point.0 as usize] + 1) as isize
}

fn valid_point(x: i8, y: i8) -> bool {
    x >= 0 && x < X && y >= 0 && y < Y
}
