#![allow(unused)]

use std::collections::HashMap;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
struct ParseError;

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error parsing polymerizations instructions")
    }
}

type Template = Vec<char>;
type InsertionRules = Vec<(Vec<char>, char)>;

#[aoc_generator(day14)]
fn input_generator(input: &str) -> Result<(Template, InsertionRules), Box<dyn Error>> {
    let mut insertion_rules: InsertionRules = Vec::new();
    let (template, insertions) = input.split_once("\n\n").ok_or(ParseError)?;

    for insertion in insertions.split("\n") {
        let (pattern, interstitial) = insertion.split_once(" -> ").ok_or(ParseError)?;

        insertion_rules.push((
            pattern.chars().collect(),
            *interstitial
                .chars()
                .collect::<Vec<char>>()
                .first()
                .ok_or(ParseError)?,
        ));
    }

    Ok((template.chars().collect(), insertion_rules))
}

#[aoc(day14, part1)]
fn part1(input: &(Template, InsertionRules)) -> usize {
    let (template, insertion_rules) = input;
    let mut template = template.clone();

    for _ in 0..10 {
        insert(&mut template, &insertion_rules);
    }

    range(count_occurrences(&template))
}

#[aoc(day14, part2)]
fn part2(input: &(Template, InsertionRules)) -> usize {
    0
}

fn insert(template: &mut Template, insertion_rules: &InsertionRules) {
    let mut operations = Vec::new();

    for (i, pair) in template.windows(2).enumerate() {
        for (pattern, interstitial) in insertion_rules {
            if pair == pattern {
                operations.push((i + 1, interstitial)); // insert after `i`
            }
        }
    }

    for (replacement, (position, interstitial)) in operations.iter().enumerate() {
        template.insert(position + replacement, **interstitial)
    }
}

fn count_occurrences(template: &Template) -> HashMap<char, usize> {
    let mut occurrences: HashMap<char, usize> = HashMap::new();

    for c in template.iter() {
        let entry = occurrences.entry(*c).or_insert(0);
        *entry += 1;
    }

    occurrences
}

fn range(occurrences: HashMap<char, usize>) -> usize {
    let mut counts = occurrences
        .iter()
        .map(|(_c, count)| *count)
        .collect::<Vec<usize>>();

    counts.sort();

    counts[counts.len() - 1] - counts[0]
}
