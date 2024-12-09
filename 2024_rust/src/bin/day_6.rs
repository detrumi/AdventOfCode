use std::collections::HashSet;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_6.txt");

fn parse() -> (Vec<Vec<char>>, (usize, usize)) {
    let map = INPUT
        .trim()
        .lines()
        .map(|line| line.chars().collect_vec())
        .collect_vec();

    let mut pos = (0, 0);
    for y in 0..map.len() {
        for x in 0..map.len() {
            if map[y][x] == '^' {
                pos = (x, y);
            }
        }
    }
    (map, pos)
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum Dir {
    North,
    East,
    South,
    West,
}

impl Dir {
    pub fn turn(self) -> Self {
        match self {
            Dir::North => Dir::East,
            Dir::East => Dir::South,
            Dir::South => Dir::West,
            Dir::West => Dir::North,
        }
    }
}

fn path() -> HashSet<(usize, usize)> {
    let (map, (mut x, mut y)) = parse();
    let end = map.len() - 1;
    let mut visited = HashSet::new();
    let mut dir = Dir::North;
    while let Some((new_x, new_y)) = match dir {
        Dir::North => (y > 0).then(|| (x, y - 1)),
        Dir::East => (x < end).then(|| (x + 1, y)),
        Dir::South => (y < end).then(|| (x, y + 1)),
        Dir::West => (x > 0).then(|| (x - 1, y)),
    } {
        visited.insert((x, y));
        if map[new_y][new_x] == '#' {
            dir = dir.turn();
        } else {
            (x, y) = (new_x, new_y);
        }
    }
    visited.insert((x, y));
    visited
}

fn part2() -> usize {
    let (map, (x, y)) = parse();
    path()
        .iter()
        .filter(|(target_x, target_y)| {
            let mut map2 = map.clone();
            map2[*target_y][*target_x] = '#';
            loops(map2, (x, y))
        })
        .count()
}

fn loops(map: Vec<Vec<char>>, (mut x, mut y): (usize, usize)) -> bool {
    let mut dir = Dir::North;
    let end = map.len() - 1;
    let mut visited = HashSet::new();
    while visited.insert((x, y, dir)) {
        let Some((new_x, new_y)) = (match dir {
            Dir::North => (y > 0).then(|| (x, y - 1)),
            Dir::East => (x < end).then(|| (x + 1, y)),
            Dir::South => (y < end).then(|| (x, y + 1)),
            Dir::West => (x > 0).then(|| (x - 1, y)),
        }) else {
            return false;
        };
        if map[new_y][new_x] == '#' {
            dir = dir.turn();
        } else {
            (x, y) = (new_x, new_y);
        }
    }
    true
}

fn main() {
    println!("Part 1: {}", path().len());
    println!("Part 2: {}", part2());
}
