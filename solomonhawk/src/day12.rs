#![allow(unused)]
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::str::FromStr;

// find distinct paths
// don't visit small caves more than once
// big caves can be visited multiple times

pub type CaveId = String;

pub struct CaveSystem {
    caves: HashMap<CaveId, Cave>,
}

#[derive(Debug)]
pub struct CaveParseError;

impl Error for CaveParseError {}

impl fmt::Display for CaveParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to parse CaveSystem")
    }
}

#[derive(Debug, PartialEq)]
pub enum CaveSize {
    Small,
    Large,
}

#[derive(Debug)]
pub struct Cave {
    id: CaveId,
    size: CaveSize,
    edges: Vec<CaveId>,
}

impl FromStr for CaveSystem {
    type Err = CaveParseError;

    fn from_str(s: &str) -> Result<CaveSystem, Self::Err> {
        let mut caves: HashMap<String, Cave> = HashMap::new();

        //     start
        //     /   \
        // c--A-----b--d
        //     \   /
        //      end

        for passage in s.split("\n") {
            let (source, dest) = passage.split_once("-").ok_or(CaveParseError)?;

            let source_name = source.to_string();
            let dest_name = dest.to_string();

            if !caves.contains_key(source) {
                caves.insert(
                    source_name.clone(),
                    Cave {
                        id: source_name.clone(),
                        size: cave_size(source),
                        edges: vec![dest_name.clone()],
                    },
                );
            } else {
                if let Some(cave) = caves.get_mut(source) {
                    cave.edges.push(dest.to_string());
                }
            }

            if !caves.contains_key(dest) {
                caves.insert(
                    dest.to_string(),
                    Cave {
                        id: dest_name.clone(),
                        size: cave_size(dest),
                        edges: vec![source_name.clone()],
                    },
                );
            } else {
                if let Some(cave) = caves.get_mut(dest) {
                    cave.edges.push(source.to_string());
                }
            }
        }

        Ok(CaveSystem { caves })
    }
}

#[aoc_generator(day12)]
pub fn input_generator(input: &str) -> Result<CaveSystem, Box<dyn Error>> {
    Ok(input.parse()?)
}

#[aoc(day12, part1)]
pub fn part1(cave_system: &CaveSystem) -> usize {
    let mut all_paths: Vec<Vec<String>> = Vec::new();

    build_paths(
        &cave_system.caves,
        String::from("start"),
        &mut all_paths,
        &mut vec![],
    );

    all_paths.len()
}

fn build_paths(
    caves: &HashMap<CaveId, Cave>,
    cave_id: CaveId,
    all_paths: &mut Vec<Vec<String>>,
    path: &mut Vec<String>,
) {
    let cave = caves.get(&cave_id).unwrap();

    path.push(cave_id.clone());

    if is_end(&cave) {
        all_paths.push(path.to_vec());
        return;
    }

    for edge_id in cave.edges.iter() {
        let edge_cave = caves.get(edge_id).unwrap();

        if is_start(&edge_cave) || is_duplicate_small_cave(&edge_cave, path.to_vec()) {
            continue;
        }

        build_paths(caves, edge_id.to_string(), all_paths, &mut path.to_vec());
    }
}

#[aoc(day12, part2)]
pub fn part2(cave_system: &CaveSystem) -> usize {
    0
}

fn is_start(cave: &Cave) -> bool {
    cave.id == "start"
}

fn is_end(cave: &Cave) -> bool {
    cave.id == "end"
}

fn is_duplicate_small_cave(cave: &Cave, path: Vec<String>) -> bool {
    cave.size == CaveSize::Small && path.contains(&cave.id)
}

fn cave_size(s: &str) -> CaveSize {
    if is_uppercase(s) {
        CaveSize::Large
    } else {
        CaveSize::Small
    }
}

fn is_uppercase(s: &str) -> bool {
    for c in s.chars() {
        if !c.is_ascii_uppercase() {
            return false;
        }
    }

    true
}
