use itertools::Itertools;

use std::collections::{HashMap, HashSet};
use std::iter;

const INPUT: &str = include_str!("../../input/day_17.txt");

fn parse(dimensions: usize) -> HashSet<Vec<i32>> {
    INPUT
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .filter(|(_x, c)| *c == '#')
                .map(move |(x, _)| {
                    let mut v = vec![x as i32, y as i32];
                    v.resize(dimensions, 0);
                    v
                })
        })
        .collect()
}

fn solve(dimensions: usize) -> usize {
    (1..=6)
        .fold(parse(dimensions), |state, _cycle| {
            let mut counts: HashMap<Vec<i32>, usize> = HashMap::new();
            for position in state.iter() {
                for neighbor in iter::repeat(-1..=1)
                    .take(dimensions)
                    .multi_cartesian_product()
                    .filter(|delta| delta.iter().any(|d| *d != 0))
                    .map(|delta| position.iter().zip(delta).map(|(p, d)| p + d).collect())
                {
                    *counts.entry(neighbor).or_insert(0) += 1;
                }
            }
            counts
                .into_iter()
                .filter(|(position, count)| {
                    *count == 3 || (*count == 2 && state.contains(position))
                })
                .map(|t| t.0)
                .collect()
        })
        .len()
}

fn main() {
    println!("Part 1: {}", solve(3));
    println!("Part 2: {}", solve(4));
}
