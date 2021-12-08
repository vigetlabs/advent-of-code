#[derive(Debug, Clone, Default)]
struct Entry {
    signals: Vec<Signal>,
    outputs: Vec<String>,
    zero: Option<Signal>,
    one: Option<Signal>,
    two: Option<Signal>,
    three: Option<Signal>,
    four: Option<Signal>,
    five: Option<Signal>,
    six: Option<Signal>,
    seven: Option<Signal>,
    eight: Option<Signal>,
    nine: Option<Signal>,
}

#[derive(Debug, Clone)]
struct Signal {
    string: String,
    number: usize,
}

#[aoc_generator(day8)]
fn input_generator(input: &str) -> Vec<Entry> {
    input
        .split("\n")
        .map(|l| {
            let (signals, outputs) = l.split_once(" | ").unwrap();
            Entry {
                signals: signals
                    .split(" ")
                    .map(|s| Signal {
                        string: s.to_string(),
                        number: signal_to_integer(s),
                    })
                    .collect(),
                outputs: outputs.split(" ").map(|s| s.to_string()).collect(),
                ..Entry::default()
            }
        })
        .collect()
}

#[aoc(day8, part1)]
fn part1(lines: &[Entry]) -> usize {
    lines
        .iter()
        .flat_map(|l| {
            l.outputs.iter().filter(|o| {
                let count = o.chars().count();
                count == 2 || count == 3 || count == 4 || count == 7
            })
        })
        .count()
}

#[aoc(day8, part2)]
fn part2(lines: &[Entry]) -> usize {
    lines.to_vec().iter_mut().map(decode_line).sum()
}

fn decode_line(entry: &mut Entry) -> usize {
    entry.one = find(&mut entry.signals, by_length(2));
    entry.four = find(&mut entry.signals, by_length(4));
    entry.seven = find(&mut entry.signals, by_length(3));
    entry.eight = find(&mut entry.signals, by_length(7));

    // signal patterns with 6 segments
    let mut sixes = filter_by_length(&mut entry.signals, 6);

    // signal patterns with 5 segments
    let mut fives = entry.signals.clone();

    // one is comprised of the top right (tr) and bottom right (br) segments
    let trbr = &entry.one.as_ref().unwrap().number;

    // subtracting the segments of one from four leaves just the middle (m) and top left (tl) segments
    let mtl = &entry.four.as_ref().unwrap().number & !trbr;

    entry.six = find(&mut sixes, |s| s.number & trbr != *trbr);
    entry.nine = find(&mut sixes, |s| s.number & mtl == mtl);
    entry.zero = find(&mut sixes, |_s| true);

    entry.three = find(&mut fives, |s| s.number & trbr == *trbr);

    // subtracting the segments in 6 from the segments in 0 leaves just the top right (tr) segment
    let tr = &entry.zero.as_ref().unwrap().number & *trbr & !entry.six.as_ref().unwrap().number;

    entry.two = find(&mut fives, |s| s.number & tr == tr);
    entry.five = find(&mut fives, |_s| true);

    entry
        .outputs
        .iter()
        .map(|o| match_signal(o, &entry))
        .collect::<Vec<&str>>()
        .join("")
        .parse::<usize>()
        .unwrap()
}

/**
 * Takes a signal (like "ebdcfa") and returns an integer value by converting the
 * characters into their ASCII values and summing them.
 */
fn signal_to_integer(signal: &str) -> usize {
    signal.chars().map(|c| 1 << (c as usize - 97)).sum()
}

/**
 * Searches a mutable vector for the first signal that matches a given predicate
 * fn. When found, removes it from the vector.
 */
fn find<T>(ns: &mut Vec<Signal>, func: T) -> Option<Signal>
where
    T: Fn(&Signal) -> bool,
{
    for (i, n) in ns.iter().enumerate() {
        if func(n) {
            return Some(ns.swap_remove(i));
        }
    }

    return None;
}

// higher-order functions, neeeat
fn by_length(len: usize) -> impl Fn(&Signal) -> bool {
    move |s: &Signal| s.string.chars().count() == len
}

/**
 * Matches a signal pattern to a decoded digit. (The sequence of signals might
 * differ but the computed integer value will always be equal).
 */
fn match_signal(signal: &String, entry: &Entry) -> &'static str {
    match signal_to_integer(signal) {
        x if x == entry.zero.as_ref().unwrap().number => "0",
        x if x == entry.one.as_ref().unwrap().number => "1",
        x if x == entry.two.as_ref().unwrap().number => "2",
        x if x == entry.three.as_ref().unwrap().number => "3",
        x if x == entry.four.as_ref().unwrap().number => "4",
        x if x == entry.five.as_ref().unwrap().number => "5",
        x if x == entry.six.as_ref().unwrap().number => "6",
        x if x == entry.seven.as_ref().unwrap().number => "7",
        x if x == entry.eight.as_ref().unwrap().number => "8",
        x if x == entry.nine.as_ref().unwrap().number => "9",
        _ => {
            panic!("Unknown number")
        }
    }
}

/**
 * Searches a mutable vector for signals with a matching length, removes them
 * from the vector and returns a new list of those signals.
 */
fn filter_by_length(ns: &mut Vec<Signal>, length: usize) -> Vec<Signal> {
    let mut result = Vec::new();
    let mut i = 0;

    loop {
        if i == ns.len() {
            break;
        }

        if ns[i].string.chars().count() == length {
            result.push(ns.swap_remove(i));
        } else {
            i += 1;
        }
    }

    result
}

/*
    Hmm... :thinking_face:

    n | count |      segments       | gfedcba
    --|-------|---------------------|---------|-----
    0 |   6   | a, b, c, e, f, g    | 1110111 | 119
    1 |   2*  | c, f                | 0100100 | 36
    2 |   5   | a, c, d, e, g       | 1011101 | 93
    3 |   5   | a, c, d, f, g       | 1101101 | 109
    4 |   4*  | b, c, d, f          | 0101110 | 46
    5 |   5   | a, b, d, f, g       | 1101011 | 107
    6 |   6   | a, b, d, e, f, g    | 1111011 | 123
    7 |   3*  | a, c, f             | 0100101 | 37
    8 |   7*  | a, b, c, d, e, f, g | 1111111 | 127
    9 |   6   | a, b, c, d, f, g    | 1101111 | 111

     a a a
    b     c
    b     c
    b     c
     d d d
    e     f
    e     f
    e     f
     g g g

    count 5: 2, 3, 5
    count 6: 0, 6, 9

    acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
    cdfeb fcadb cdfeb cdbaf

    find chars in 1, 4, 7, 8
        1: ab
        4: eafb
        7: dab
        8: acedgfb

    identify the right 2 segments (vertical line in 7)
        tr/br: "ab"

    identify the middle and top left segment pair (parts of 4 minus tr/br)
        m/tl: "ef"

    find candidates for 0, 6, 9 (length = 6)
        cefabd cdfgeb cagedb

        the one without the right 2 (tr/br) matching is "6"
            cdfgeb = 6 (it does not have both tr/br "ab")

        the one without both m/tl is "0"
            cagedb = 0 (it does not have both m/tl "ef")

        the last one is "9"
            cefabd = 9

    find candidates for 2, 3, 5 (length = 5)
        cdfbe gcdfa fbcad

        the one with the right 2 matching is "3"
            fbcad = 3 (it has tr/br "ab")

        of the tr/br signals, the one in "0" that's not in "6" is tr
            "0" cagedb => filter tr/br ("ab") => ab
            "6" cdfgeb => filter tr/br ("b") => b
            signal a is tr

        of the remaining 2 entries, the one with tr is "2"
            gcdfa = 2

        the last entry is "5"
            cdfbe = 5


    entries = acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab

    one = entries.find(e => e.length = 2)
    four = entries.find(e => e.length = 4)
    seven = entries.find(e => e.length = 3)
    eight = entries.find(e => e.length = 7)

    fivers = entries.filter(e => e.length = 5)

    trbr = binary_from(one) // 0000011
    mtl = binary_from(four) without trbr // 0110011 - 0000011 = 0110000 (a & !b)

    six = fivers.take(n => n & trbr != trbr)
    nine = fivers.take(n => n & mtl == mtl)
    zero = fivers.last()

    sixers = entries.filter(e => e.length = 6)

    three = sixers.take(n => n & trbr == trbr)

    tr = zero & trbr & !six

    two = sixers.take(n => n & tr)
    five = sixers.last()
*/
