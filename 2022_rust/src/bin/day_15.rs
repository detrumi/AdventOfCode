use std::collections::HashSet;

use itertools::Itertools;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

const INPUT: &str = include_str!("../../input/day_15.txt");

fn parse() -> Vec<((isize, isize), (isize, isize))> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let parts = line
                .split_ascii_whitespace()
                .map(|n| n.parse::<isize>().unwrap())
                .collect_vec();
            let sensor = (parts[0], parts[1]);
            let beacon = (parts[2], parts[3]);
            (sensor, beacon)
        })
        .collect_vec()
}

fn part1(y: isize) -> usize {
    let mut positions = HashSet::new();
    let mut objects = HashSet::new();
    for (sensor, beacon) in parse() {
        let dist = (sensor.0 - beacon.0).abs() + (sensor.1 - beacon.1).abs();
        if sensor.1 == y {
            objects.insert(sensor.0);
        }
        if beacon.1 == y {
            objects.insert(beacon.0);
        }

        let dy = (sensor.1 - y).abs();
        let dx = dist - dy;
        if dx >= 0 {
            for x in sensor.0 - dx..=sensor.0 + dx {
                positions.insert(x);
            }
        }
    }
    positions.difference(&objects).count()
}

fn part2(max: usize) {
    let rows = parse();
    (0..=max as isize).into_par_iter().for_each(|y| {
        let mut possibilities = vec![true; max + 1];
        let mut count = 0;
        for (sensor, beacon) in &rows {
            let dist = (sensor.0 - beacon.0).abs() + (sensor.1 - beacon.1).abs();
            let dx = dist - (sensor.1 - y).abs();
            if dx >= 0 {
                for x in (sensor.0 - dx).max(0)..=(sensor.0 + dx).min(max as isize) {
                    let b = unsafe { possibilities.get_unchecked_mut(x as usize) };
                    if *b {
                        count += 1;
                        *b = false;
                    }
                }
            }
        }
        if count != 4_000_001 {
            if let Some(x) = possibilities.iter().position(|&b| b) {
                println!("Part 2: {}", 4_000_000 * x as isize + y);
            }
        }
    });
}

fn main() {
    println!("Part 1: {}", part1(2_000_000));
    part2(4_000_000);
}
