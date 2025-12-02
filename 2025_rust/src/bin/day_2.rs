#![feature(new_range_api)]

use std::ops::RangeInclusive;

const INPUT: &str = include_str!("../../input/day_2.txt");

fn parse() -> Vec<RangeInclusive<usize>> {
    INPUT
        .trim()
        .split(",")
        .take_while(|s| !s.is_empty())
        .map(|s| s.split_once("-").unwrap())
        .map(|(from, to)| from.parse().unwrap()..=to.parse().unwrap())
        .collect()
}

fn part1() -> usize {
    parse()
        .into_iter()
        .map(|range| {
            range
                .filter(|n| {
                    let s = n.to_string();
                    s[0..s.len() / 2] == s[s.len() / 2..]
                })
                .sum::<usize>()
        })
        .sum()
}

fn part2() -> usize {
    parse()
        .into_iter()
        .map(|range| {
            range
                .filter(|n| {
                    let s = n.to_string();
                    (1..=s.len() / 2).any(|len| s == s[..len].repeat(s.len() / len))
                })
                .sum::<usize>()
        })
        .sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
