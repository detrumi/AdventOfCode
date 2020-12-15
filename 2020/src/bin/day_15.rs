use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_15.txt");

fn solve(nth: usize) -> usize {
    let mut said: HashMap<usize, (Option<usize>, usize)> = INPUT
        .split(',')
        .enumerate()
        .map(|(i, n)| (n.parse().unwrap(), (None, i + 1)))
        .collect();
    (said.len() + 1..=nth).fold(0, |mut n, turn| {
        n = said.get(&n).map_or(n, |(a, b)| a.map_or(0, |a| b - a));
        said.insert(n, (said.get(&n).map(|turns| turns.1), turn));
        n
    })
}

fn main() {
    println!("Part 1: {}", solve(2020));
    println!("Part 2: {}", solve(30_000_000));
}
