#![feature(extend_one)]

use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_10.txt");

fn parse() -> Vec<Vec<u32>> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap_or(100))
                .collect()
        })
        .collect()
}

fn solve<T: Extend<(usize, usize)> + Default + IntoIterator>() -> usize {
    let map = parse();
    let mut result = 0;
    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if map[y][x] == 0 {
                let mut heads = T::default();
                hike(&map, x, y, &mut heads);
                result += heads.into_iter().count();
            }
        }
    }
    result
}

fn hike(map: &Vec<Vec<u32>>, x: usize, y: usize, heads: &mut impl Extend<(usize, usize)>) {
    for n in [
        (x > 0).then(|| (x - 1, y)),
        (x < map[0].len() - 1).then(|| (x + 1, y)),
        (y > 0).then(|| (x, y - 1)),
        (y < map.len() - 1).then(|| (x, y + 1)),
    ] {
        if let Some((nx, ny)) = n
            && map[ny][nx] == map[y][x] + 1
        {
            if map[ny][nx] == 9 {
                heads.extend_one((nx, ny));
            } else {
                hike(map, nx, ny, heads);
            }
        }
    }
}

fn main() {
    println!("Part 1: {}", solve::<HashSet<_>>());
    println!("Part 2: {}", solve::<Vec<_>>());
}
