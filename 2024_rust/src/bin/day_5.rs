use std::{cmp::Ordering, collections::HashSet};

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_5.txt");

fn parse() -> (HashSet<(usize, usize)>, impl Iterator<Item = Vec<usize>>) {
    let chunks = INPUT.trim().split("\n\n").collect_vec();
    let orderings = chunks[0]
        .lines()
        .map(|l| {
            l.split('|')
                .map(|n| n.parse().unwrap())
                .next_tuple()
                .unwrap()
        })
        .collect();
    let pages = chunks[1]
        .lines()
        .map(|l| l.split(',').map(|n| n.parse().unwrap()).collect());
    (orderings, pages)
}

fn part1() -> usize {
    let (orderings, pages) = parse();
    pages
        .filter(|page| page.is_sorted_by(|&a, &b| orderings.contains(&(a, b))))
        .map(|page| page[page.len() / 2])
        .sum()
}

fn part2() -> usize {
    let (orderings, pages) = parse();
    pages
        .map(|mut page| {
            page.sort_by(|&a, &b| {
                if orderings.contains(&(a, b)) {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            });
            page[page.len() / 2]
        })
        .sum::<usize>()
        - part1()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
