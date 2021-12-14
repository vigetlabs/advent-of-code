#![allow(unused)]

use itertools::Itertools;
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
type InsertionRules = Vec<(Template, char)>;
type Mapping = HashMap<Template, usize>;

#[aoc_generator(day14)]
fn input_generator(input: &str) -> Result<(Template, InsertionRules), Box<dyn Error>> {
    let mut insertion_rules: InsertionRules = Vec::new();
    let (template, insertions) = input.split_once("\n\n").ok_or(ParseError)?;

    for insertion in insertions.split("\n") {
        let (pattern, interstitial) = insertion.split_once(" -> ").ok_or(ParseError)?;

        insertion_rules.push((
            pattern.chars().collect(),
            interstitial.chars().nth(0).ok_or(ParseError)?,
        ));
    }

    Ok((template.chars().collect(), insertion_rules))
}

#[aoc(day14, part1)]
fn part1(input: &(Template, InsertionRules)) -> usize {
    let (template, insertion_rules) = input;
    let mut mapping: Mapping = initialize_mapping(&template, &insertion_rules);

    for _ in 0..10 {
        replace(&mut mapping, &insertion_rules);
    }

    range(count_occurrences(&mapping))
}

#[aoc(day14, part2)]
fn part2(input: &(Template, InsertionRules)) -> usize {
    let (template, insertion_rules) = input;
    let mut mapping: Mapping = initialize_mapping(&template, &insertion_rules);

    for _ in 0..40 {
        replace(&mut mapping, &insertion_rules);
    }

    range(count_occurrences(&mapping))
}

fn initialize_mapping(template: &Template, insertion_rules: &InsertionRules) -> Mapping {
    // create a mapping of all unique pairs of chars to their associated counts
    // { ['N', 'C']: 1, ['N', 'N']: 0, .. }
    let mut mapping: Mapping = insertion_rules
        .iter()
        .flat_map(|(pattern, _)| pattern.clone())
        .unique()
        .flat_map(|c| vec![c, c])
        .permutations(2)
        .unique()
        .map(|t| (t, 0))
        .collect();

    // populate the mapping with pair occurence counts of starting template
    for pair in template.windows(2) {
        if let Some(x) = mapping.get_mut(pair) {
            *x += 1;
        }
    }

    mapping
}

fn replace(mapping: &mut Mapping, insertion_rules: &InsertionRules) {
    let mut operations: Vec<(Template, isize)> = Vec::new();

    for (pair, count) in mapping.iter().filter(|(_, &count)| count > 0) {
        for (pattern, interstitial) in insertion_rules {
            if pair == pattern {
                // after replacement, the pair will be replaced with two patterns
                let before = vec![pair[0], *interstitial];
                let after = vec![*interstitial, pair[1]];

                // decrement the pair (e.g. [N, N])
                operations.push((pair.to_vec(), *count as isize * -1));
                // increment the new starting pair (e.g. [N, C])
                operations.push((before, *count as isize));
                // increment the new ending pair (e.g. [C, N])
                operations.push((after, *count as isize));
            }
        }
    }

    for (pair, adjustment) in operations.iter() {
        if let Some(x) = mapping.get_mut(pair) {
            *x = (*x as isize + adjustment) as usize;
        }
    }
}

fn count_occurrences(mapping: &Mapping) -> HashMap<char, usize> {
    let mut occurrences: HashMap<char, usize> = HashMap::new();

    for (pair, count) in mapping.iter() {
        // why does this work? 🤷🏻‍♂️
        let entry = occurrences.entry(pair[1]).or_insert(0);
        *entry += count;
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
