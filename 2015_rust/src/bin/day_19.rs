use std::collections::{BinaryHeap, HashMap, HashSet};

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_19.txt");

fn rename(s: String, names: &mut Vec<String>) -> usize {
    if let Some(n) = names.iter().position(|v| **v == s) {
        n
    } else {
        names.insert(names.len(), s);
        names.len() - 1
    }
}

fn split_molecules(s: &str, names: &mut Vec<String>) -> Vec<usize> {
    let mut result = vec![];
    let mut it = s.chars().peekable();
    while let Some(c) = it.next() {
        if c.is_ascii_lowercase() && c != 'e' {
            continue;
        }

        let mut molecule = c.to_string();
        match it.peek() {
            Some(c2) if c2.is_ascii_lowercase() => molecule.push(*c2),
            _ => (),
        };

        result.push(rename(molecule, names));
    }
    result
}

#[derive(Debug)]
struct Ruleset {
    rules: HashMap<usize, Vec<Vec<usize>>>,
    names: Vec<String>,
    target: Vec<usize>,
}

impl Ruleset {
    pub fn new() -> Self {
        let (rule_part, target_part) = INPUT.trim().split("\n\n").collect_tuple().unwrap();

        let mut names = vec![];
        let mut rules: HashMap<usize, Vec<Vec<usize>>> = HashMap::new();
        for line in rule_part.lines() {
            let (from, to) = line
                .split(" => ")
                .map(|s| split_molecules(s, &mut names))
                .collect_tuple()
                .unwrap();
            rules.entry(from[0]).or_default().push(to);
        }
        let target = split_molecules(target_part, &mut names);

        Self {
            rules,
            names,
            target,
        }
    }

    #[allow(dead_code)]
    fn display(&self, molecule: &[usize]) -> String {
        molecule
            .iter()
            .map(|n| self.names[*n].clone())
            .collect::<String>()
    }

    pub fn part1(&self) -> usize {
        let mut found = HashSet::new();
        for i in 0..self.target.len() {
            if let Some(replacements) = self.rules.get(&self.target[i]) {
                for replacement in replacements {
                    found.insert([&self.target[0..i], replacement, &self.target[i + 1..]].concat());
                }
            }
        }
        found.len()
    }

    pub fn part2(&self) -> usize {
        let mut targets = BinaryHeap::new();
        targets.push(Search {
            target: self.target.clone(),
            steps: 0,
        });
        while let Some(Search { target, steps }) = targets.pop() {
            for (&from, to) in &self.rules {
                for replacements in to {
                    let len = replacements.len();
                    if len > target.len() {
                        continue;
                    }
                    for i in 0..=target.len() - len {
                        if target[i..i + len] == *replacements {
                            let shortened = [&target[0..i], &[from], &target[i + len..]].concat();
                            if shortened == vec![self.names.len() - 1] {
                                return steps + 1;
                            }
                            targets.push(Search {
                                target: shortened,
                                steps: steps + 1,
                            });
                        }
                    }
                }
            }
        }
        unreachable!()
    }
}

struct Search {
    pub target: Vec<usize>,
    pub steps: usize,
}

impl PartialEq for Search {
    fn eq(&self, other: &Self) -> bool {
        self.target.len() == other.target.len()
    }
}
impl Eq for Search {}

impl PartialOrd for Search {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.target.len().partial_cmp(&self.target.len())
    }
}

impl Ord for Search {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.target.len().cmp(&self.target.len())
    }
}

fn main() {
    let ruleset = Ruleset::new();
    println!("Part 1: {}", ruleset.part1());
    println!("Part 2: {}", ruleset.part2());
}
