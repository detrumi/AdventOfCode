use std::collections::{BinaryHeap, HashSet};

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_12.txt");

fn parse() -> Vec<String> {
    INPUT
        .trim()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect()
}

#[derive(Eq, Debug)]
struct Route {
    pub distance: usize,
    pub height: u32,
    pub pos: (usize, usize),
    pub path: Vec<(usize, usize)>,
}

impl PartialEq for Route {
    fn eq(&self, other: &Self) -> bool {
        self.height == other.height && self.distance == other.distance
    }
}

impl PartialOrd for Route {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Route {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .distance
            .cmp(&self.distance)
            .then(self.height.cmp(&other.height))
    }
}

pub fn neighbors(pos: (usize, usize), grid: &Vec<Vec<char>>) -> Vec<(usize, usize)> {
    let mut neighbors = vec![];
    for (dx, dy) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
        if pos.1 as isize + dy >= 0
            && pos.1 as isize + dy < grid.len() as isize
            && pos.0 as isize + dx >= 0
            && pos.0 as isize + dx < grid[0].len() as isize
        {
            neighbors.push((
                (pos.0 as isize + dx) as usize,
                (pos.1 as isize + dy) as usize,
            ));
        }
    }
    neighbors
}

fn part1() -> usize {
    let mut grid = INPUT
        .trim()
        .lines()
        .map(|line| line.chars().collect_vec())
        .collect_vec();
    let mut start = (0, 0);
    let mut end = (0, 0);
    for (y, line) in grid.iter().enumerate() {
        for (x, &c) in line.iter().enumerate() {
            match c {
                'S' => start = (x, y),
                'E' => end = (x, y),
                _ => (),
            }
        }
    }
    let mut routes = BinaryHeap::new();
    routes.push(Route {
        distance: 0,
        height: 0,
        pos: start,
        path: vec![(start.0, start.1)],
    });
    grid[start.1][start.0] = 'a';
    let mut visited = HashSet::new();
    while let Some(Route {
        distance,
        height,
        pos,
        path,
    }) = routes.pop()
    {
        for (nx, ny) in neighbors(pos, &grid) {
            if (nx, ny) == end {
                if height < 24 {
                    continue;
                }
                let set: HashSet<_> = path.iter().collect();
                for y in 0..grid.len() {
                    for x in 0..grid[0].len() {
                        print!("{}", if set.contains(&(x, y)) { '#' } else { '.' });
                    }
                    println!();
                }
                return distance + 1;
            }

            let new_height = grid[pos.1][pos.0] as u32 - 'a' as u32;
            if new_height <= height + 1 && visited.insert((nx, ny, distance, height)) {
                let mut new_path = path.clone();
                new_path.push((nx, ny));
                routes.push(Route {
                    distance: distance + 1,
                    height: new_height,
                    pos: (nx, ny),
                    path: new_path,
                });
            }
        }
    }
    panic!()
}

fn part2() -> usize {
    let mut grid = INPUT
        .trim()
        .lines()
        .map(|line| line.chars().collect_vec())
        .collect_vec();
    let mut start = (0, 0);
    let mut end = (0, 0);
    for (y, line) in grid.iter().enumerate() {
        for (x, &c) in line.iter().enumerate() {
            match c {
                'S' => start = (x, y),
                'E' => end = (x, y),
                _ => (),
            }
        }
    }
    grid[start.1][start.0] = 'a';
    let mut best = usize::MAX;
    for sy in 0..grid.len() {
        for sx in 0..grid[0].len() {
            if grid[sy][sx] != 'a' {
                continue;
            }

            let mut routes = BinaryHeap::new();
            routes.push(Route {
                distance: 0,
                height: 0,
                pos: (sx, sy),
                path: vec![(sx, sy)],
            });
            let mut visited = HashSet::new();
            'outer: while let Some(Route {
                distance,
                height,
                pos,
                path,
            }) = routes.pop()
            {
                for (nx, ny) in neighbors(pos, &grid) {
                    if (nx, ny) == end {
                        if height < 24 {
                            continue;
                        }
                        best = best.min(distance + 1);
                        break 'outer;
                    }

                    let new_height = grid[pos.1][pos.0] as u32 - 'a' as u32;
                    if new_height <= height + 1 && visited.insert((pos, nx, ny)) {
                        let mut new_path = path.clone();
                        new_path.push((nx, ny));
                        routes.push(Route {
                            distance: distance + 1,
                            height: new_height,
                            pos: (nx, ny),
                            path: new_path,
                        });
                    }
                }
            }
        }
    }

    best
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
