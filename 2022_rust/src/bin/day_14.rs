use std::collections::{HashMap, HashSet};

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_14.txt");

fn solve(part: usize) -> usize {
    let mut grid = HashSet::new();
    for line in INPUT.trim().lines() {
        let parts = line
            .split(" -> ")
            .map(|s| {
                let (x, y) = s.split_once(",").unwrap();
                (x.parse::<isize>().unwrap(), y.parse::<isize>().unwrap())
            })
            .collect_vec();
        for v in parts.windows(2) {
            let (from, to) = (v[0], v[1]);
            let dx = (to.0 - from.0).signum();
            let dy = (to.1 - from.1).signum();
            let mut pos = (from.0, from.1);
            grid.insert(pos);
            while pos != to {
                pos = (pos.0 + dx, pos.1 + dy);
                grid.insert(pos);
            }
        }
    }
    let minimums: HashMap<isize, isize> = {
        let min_x = grid.iter().map(|p| p.0).min().unwrap();
        let max_x = grid.iter().map(|p| p.0).max().unwrap();
        (min_x - 1..=max_x + 1)
            .map(|x| {
                (
                    x,
                    grid.iter()
                        .filter(|p| p.0 == x)
                        .map(|p| p.1)
                        .max()
                        .unwrap_or_default(),
                )
            })
            .collect()
    };
    let max_y = grid.iter().map(|p| p.1).max().unwrap() + 1;

    for sand_dropped in 0.. {
        let mut pos = (500, 0);
        loop {
            if part == 1 && pos.1 > minimums[&pos.0] {
                return sand_dropped;
            }

            let down = (pos.0, pos.1 + 1);
            let down_left = (pos.0 - 1, pos.1 + 1);
            let down_right = (pos.0 + 1, pos.1 + 1);
            if part == 2 && pos.1 == max_y {
                grid.insert(pos);
                break;
            } else if !grid.contains(&down) {
                pos = down;
            } else if !grid.contains(&down_left) {
                pos = down_left;
            } else if !grid.contains(&down_right) {
                pos = down_right;
            } else if part == 2 && pos == (500, 0) {
                return sand_dropped + 1;
            } else {
                grid.insert(pos);
                break;
            }
        }
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", solve(1));
    println!("Part 2: {}", solve(2));
}
