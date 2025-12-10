use std::ops::RangeInclusive;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_9.txt");

#[derive(Clone, Copy, PartialEq, Eq, Default, Hash, Debug)]
struct Pos {
    pub x: usize,
    pub y: usize,
}

fn parse() -> Vec<Pos> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let (a, b) = line.split_once(',').unwrap();
            Pos {
                x: a.parse().unwrap(),
                y: b.parse().unwrap(),
            }
        })
        .collect()
}

fn part1() -> usize {
    let input = parse();
    let mut best = 0;
    for i in 0..input.len() - 1 {
        for j in i + 1..input.len() {
            best = best
                .max((input[i].x.abs_diff(input[j].x) + 1) * (input[i].y.abs_diff(input[j].y) + 1))
        }
    }
    best
}

fn range(a: usize, b: usize) -> RangeInclusive<usize> {
    a.min(b)..=a.max(b)
}

fn crosses(a: (Pos, Pos), b: (Pos, Pos)) -> bool {
    if b.0.x == b.1.x && range(a.0.x, a.1.x).contains(&b.0.x) {
        let range_a = range(a.0.y, a.1.y);
        let range_b = range(b.0.y, b.1.y);
        if range_a.contains(range_b.start()) || range_a.contains(range_b.end()) {
            return true;
        }
    } else if b.0.y == b.1.y && range(a.0.y, a.1.y).contains(&b.0.y) {
        let range_a = range(a.0.x, a.1.x);
        let range_b = range(b.0.x, b.1.x);
        if range_a.contains(range_b.start()) || range_a.contains(range_b.end()) {
            return true;
        }
    }
    false
}

fn part2() -> usize {
    let input = parse();
    let mut lines = vec![(input[input.len() - 1], input[0])];
    lines.extend(input.iter().copied().tuple_windows::<(Pos, Pos)>());
    let mut best = 0;

    for i in 0..input.len() - 1 {
        eprintln!("i = {:?}", i);
        for j in i + 1..input.len() {
            if lines.iter().all(|line| {
                line.0 == input[i]
                    || line.0 == input[j]
                    || line.1 == input[i]
                    || line.1 == input[j]
                    || !crosses((input[i], input[j]), *line)
            }) {
                best = best.max(
                    (input[i].x.abs_diff(input[j].x) + 1) * (input[i].y.abs_diff(input[j].y) + 1),
                )
            }
        }
    }
    best
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2()); // 4525501422 too high
}
