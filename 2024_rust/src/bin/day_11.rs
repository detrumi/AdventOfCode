use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_11.txt");

fn parse() -> HashMap<usize, usize> {
    INPUT
        .trim()
        .split_ascii_whitespace()
        .map(|n| n.parse().unwrap())
        .map(|n| (n, 1))
        .collect()
}

fn solve(blinks: usize) -> usize {
    let mut stones = parse();
    for _blink in 0..blinks {
        let mut new_stones = HashMap::new();
        for (n, count) in stones {
            let s = n.to_string();
            match n {
                0 => *new_stones.entry(1).or_default() += count,
                _ if s.len() % 2 == 0 => {
                    let (l, r) = s.split_at(s.len() / 2);
                    *new_stones.entry(l.parse().unwrap()).or_default() += count;
                    *new_stones.entry(r.parse().unwrap()).or_default() += count;
                }
                _ => *new_stones.entry(2024 * n).or_default() += count,
            }
        }
        stones = new_stones;
    }
    stones.values().sum()
}

fn main() {
    println!("Part 1: {}", solve(25));
    println!("Part 2: {}", solve(75));
}
