#![feature(new_range_api)]

use std::ops::RangeInclusive;

const INPUT: &str = include_str!("../../input/day_5.txt");

fn parse() -> (Vec<RangeInclusive<usize>>, Vec<usize>) {
    let (ranges, ids) = INPUT.trim().split_once("\n\n").unwrap();
    let ranges = ranges
        .lines()
        .map(|line| {
            let (l, r) = line.split_once("-").unwrap();
            l.parse().unwrap()..=r.parse().unwrap()
        })
        .collect();
    let ids = ids.lines().map(|line| line.parse().unwrap()).collect();
    (ranges, ids)
}

fn part1() -> usize {
    let (ranges, ids) = parse();
    ids.iter()
        .filter(|id| ranges.iter().any(|r| r.contains(&id)))
        .count()
}

fn combine(ranges: Vec<RangeInclusive<usize>>) -> Vec<RangeInclusive<usize>> {
    let mut combined: Vec<RangeInclusive<usize>> = vec![];
    for range in ranges {
        if let Some(i) = combined.iter().position(|r| {
            r.contains(range.start())
                || r.contains(range.end())
                || range.contains(r.start())
                || range.contains(r.end())
        }) {
            combined[i] =
                *range.start().min(combined[i].start())..=*range.end().max(combined[i].end())
        } else {
            combined.push(range);
        }
    }
    combined
}

fn part2() -> usize {
    let (mut ranges, _ids) = parse();
    let mut len = 0;
    while ranges.len() != len {
        len = ranges.len();
        ranges = combine(ranges);
    }
    ranges.into_iter().map(|range| range.count()).sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
