use itertools::Itertools;

use std::collections::HashSet;
use std::iter;

const INPUT: &str = include_str!("../../input/day_17.txt");

fn solve(dimensions: usize) -> usize {
    let state: HashSet<Vec<i32>> = INPUT
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
        .collect();

    (1..=6)
        .fold(state, |state, cycle| {
            iter::repeat(-cycle..INPUT.lines().count() as i32 + cycle)
                .take(dimensions)
                .multi_cartesian_product()
                .filter(|positions| {
                    let count = iter::repeat(-1..=1)
                        .take(dimensions)
                        .multi_cartesian_product()
                        .filter(|deltas| deltas.iter().any(|d| *d != 0))
                        .filter(|deltas| {
                            state.contains(
                                &positions
                                    .iter()
                                    .zip(deltas)
                                    .map(|(p, d)| p + d)
                                    .collect::<Vec<i32>>(),
                            )
                        })
                        .count();
                    count == 3 || (count == 2 && state.contains(positions))
                })
                .collect()
        })
        .len()
}

fn main() {
    println!("Part 1: {}", solve(3));
    println!("Part 2: {}", solve(4));
}
