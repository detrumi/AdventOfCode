use std::collections::HashMap;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_14.txt");

fn parse() -> (String, HashMap<String, char>) {
    let parts: Vec<_> = INPUT.trim().split("\n\n").collect();
    let rules: HashMap<String, char> = parts[1]
        .lines()
        .map(|line| {
            let (a, b) = line.split_once(" -> ").unwrap();
            (a.to_string(), b.chars().next().unwrap())
        })
        .collect();

    (parts[0].to_string(), rules)
}

fn part1() -> usize {
    let (mut polymer, rules) = parse();
    for _step in 0..10 {
        let mut new_polymer = String::new();
        for (a, b) in polymer.chars().tuple_windows() {
            new_polymer.push(a);
            if let Some(c) = rules.get(&format!("{}{}", a, b)) {
                new_polymer.push(*c);
            }
        }
        new_polymer.push(polymer.chars().last().unwrap());
        polymer = new_polymer;
    }

    let groups: Vec<_> = polymer
        .chars()
        .sorted()
        .group_by(|c| *c)
        .into_iter()
        .map(|g| g.1.count())
        .sorted()
        .collect();

    groups.last().unwrap() - groups[0]
}

fn expand(s: &str, rules: &HashMap<String, char>) -> String {
    let mut result = String::new();
    for (a, b) in s.chars().tuple_windows() {
        result.push(a);
        if let Some(c) = rules.get(&format!("{}{}", a, b)) {
            result.push(*c);
        }
    }
    result.push(s.chars().last().unwrap());
    result
}

fn expand2(s: &str, rules: &HashMap<String, String>) -> String {
    let mut result = s.chars().next().unwrap().to_string();
    for (a, b) in s.chars().tuple_windows() {
        if let Some(s) = rules.get(&format!("{}{}", a, b)) {
            result.push_str(&s[1..]);
        }
    }
    result
}

fn part2() -> usize {
    let (polymer, rules) = parse();

    let mut expansions: HashMap<String, String> = HashMap::new();
    for (from, to) in &rules {
        let (a, b) = from.chars().collect_tuple().unwrap();
        expansions.insert(from.clone(), format!("{}{}{}", a, to, b));
    }

    for _step in 2..=20 {
        for v in expansions.values_mut() {
            *v = expand(v, &rules);
        }
    }

    let expansion_counts: HashMap<String, HashMap<char, usize>> = expansions
        .iter()
        .map(|(from, to)| {
            let mut result: HashMap<char, usize> = HashMap::new();
            for c in to.chars().skip(1) {
                *result.entry(c).or_default() += 1;
            }
            (from.clone(), result)
        })
        .collect();

    let mut counts: HashMap<char, usize> = HashMap::new();

    let polymer = expand2(&polymer, &expansions);
    counts.insert(polymer.chars().next().unwrap(), 1);
    for (a, b) in polymer.chars().tuple_windows() {
        for (c, n) in expansion_counts.get(&format!("{}{}", a, b)).unwrap() {
            *counts.entry(*c).or_default() += n;
        }
    }

    let (_max_char, max) = counts.iter().max_by_key(|(_c, n)| *n).unwrap();
    let (_min_char, min) = counts.iter().min_by_key(|(_c, n)| *n).unwrap();
    max - min
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
