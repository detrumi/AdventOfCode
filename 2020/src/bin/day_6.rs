#![feature(iterator_fold_self)]

use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_6.txt");

fn part1() -> usize {
    INPUT
        .split("\n\n")
        .map(|block| {
            block
                .lines()
                .flat_map(|line| line.chars())
                .collect::<HashSet<_>>()
                .len()
        })
        .sum()
}

fn part2() -> usize {
    INPUT
        .split("\n\n")
        .map(|block| {
            block
                .lines()
                .map(|line| line.chars().collect::<HashSet<_>>())
                .fold_first(|a, b| a.intersection(&b).cloned().collect())
                .unwrap()
                .len()
        })
        .sum()
}
fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
