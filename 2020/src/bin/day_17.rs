use itertools::Itertools;

use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_17.txt");

fn solve(dimensions: usize) -> usize {
    let mut old: HashSet<Vec<i32>> = INPUT
        .lines()
        .enumerate()
        .flat_map(|(y, l)| {
            l.chars()
                .enumerate()
                .filter(|(_x, c)| *c == '#')
                .map(move |(x, _)| {
                    let mut v = vec![x as i32, y as i32];
                    v.resize(dimensions, 0);
                    v
                })
        })
        .collect();

    let len = INPUT.lines().count() as i32;
    for cycle in 1..=6 {
        let mut new = HashSet::new();
        for positions in (0..dimensions)
            .map(|_| -cycle..len + cycle)
            .multi_cartesian_product()
        {
            let count = (0..dimensions)
                .map(|_| -1..=1)
                .multi_cartesian_product()
                .filter(|deltas| deltas.iter().any(|d| *d != 0))
                .filter(|deltas| {
                    old.contains(
                        &positions
                            .iter()
                            .zip(deltas)
                            .map(|(p, d)| p + d)
                            .collect::<Vec<i32>>(),
                    )
                })
                .count();
            if count == 3 || (count == 2 && old.contains(&positions)) {
                new.insert(positions);
            }
        }
        old = new;
    }
    old.len()
}

fn main() {
    println!("Part 1: {}", solve(3));
    println!("Part 2: {}", solve(4));
}
