use std::{collections::HashMap, path::PathBuf};

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_7.txt");

fn parse() -> HashMap<String, usize> {
    let mut files: HashMap<String, usize> = HashMap::new();
    let mut dir = PathBuf::new();
    for line in INPUT.trim().lines() {
        let parts = line.split_ascii_whitespace().collect_vec();
        match (parts[0], parts[1]) {
            ("$", "cd") => match parts[2] {
                "/" => dir = PathBuf::new(),
                ".." => {
                    dir.pop();
                }
                d => dir.push(d),
            },
            ("$", "ls") => (),
            ("dir", _) => (),
            _ => {
                let size: usize = parts[0].parse().unwrap();
                for d in dir.ancestors() {
                    *files.entry(d.to_string_lossy().to_string()).or_default() += size;
                }
            }
        }
    }
    files
}

fn part1() -> usize {
    parse().values().filter(|&&size| size <= 100000).sum()
}

fn part2() -> usize {
    let files = parse();
    let remaining = 30000000 - (70000000 - files[""]);
    *files.values().filter(|&&n| n >= remaining).min().unwrap()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
