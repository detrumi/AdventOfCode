use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_6.txt");

fn solve(n: usize) -> usize {
    for (i, window) in INPUT.trim().as_bytes().windows(n).enumerate() {
        if window.iter().collect::<HashSet<_>>().len() == n {
            return i + n;
        }
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", solve(4));
    println!("Part 2: {}", solve(14));
}
