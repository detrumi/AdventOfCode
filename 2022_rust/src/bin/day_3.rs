use std::collections::HashSet;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_3.txt");

fn value(c: &char) -> usize {
    if c.is_uppercase() {
        *c as usize - 'A' as usize + 26 + 1
    } else {
        *c as usize - 'a' as usize + 1
    }
}

fn part1() -> usize {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let (left, right) = line.split_at(line.len() / 2);
            let left: HashSet<char> = left.chars().collect();
            let right: HashSet<char> = right.chars().collect();
            left.intersection(&right).map(value).sum::<usize>()
        })
        .sum()
}

fn part2() -> Option<usize> {
    let mut result = 0;
    for group in &INPUT
        .trim()
        .lines()
        .map(|line| line.chars().collect::<HashSet<char>>())
        .chunks(3)
    {
        let set = group.reduce(|acc, s| acc.intersection(&s).cloned().collect())?;
        result += value(set.iter().next()?);
    }
    Some(result)
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2().unwrap());
}
