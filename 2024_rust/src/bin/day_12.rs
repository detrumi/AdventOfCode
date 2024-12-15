#![feature(let_chains)]

use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_12.txt");

fn parse() -> Vec<Vec<char>> {
    INPUT
        .trim()
        .lines()
        .map(|line| line.chars().collect())
        .collect()
}

fn neighbors(farm: &Vec<Vec<char>>, x: usize, y: usize) -> [Option<(usize, usize)>; 4] {
    [
        (x > 0).then(|| (x - 1, y)),
        (x < farm[0].len() - 1).then(|| (x + 1, y)),
        (y > 0).then(|| (x, y - 1)),
        (y < farm.len() - 1).then(|| (x, y + 1)),
    ]
}

fn part1() -> usize {
    let farm = parse();
    let mut result = 0;
    let mut visited = vec![vec![false; farm[0].len()]; farm.len()];
    for y in 0..farm.len() {
        for x in 0..farm[0].len() {
            if !visited[y][x] {
                let (a, p) = walk(&farm, (x, y), &mut visited);
                result += a * p;
            }
        }
    }
    result
}

fn walk(
    farm: &Vec<Vec<char>>,
    (x, y): (usize, usize),
    visited: &mut Vec<Vec<bool>>,
) -> (usize, usize) {
    let plot = farm[y][x];
    visited[y][x] = true;
    let mut area = 1;
    let mut perimeter = 0;
    for n in neighbors(farm, x, y) {
        if let Some((nx, ny)) = n {
            if farm[ny][nx] == plot {
                if !visited[ny][nx] {
                    let (a, p) = walk(farm, (nx, ny), visited);
                    area += a;
                    perimeter += p;
                }
            } else {
                perimeter += 1;
            }
        } else {
            perimeter += 1;
        }
    }
    (area, perimeter)
}

fn part2() -> usize {
    let farm = parse();
    let mut result = 0;
    let mut visited = vec![vec![false; farm[0].len()]; farm.len()];
    for y in 0..farm.len() {
        for x in 0..farm[0].len() {
            if !visited[y][x] {
                let (area, perimeters) = walk2(&farm, (x, y), &mut visited);
                let mut total_sides = 0;
                for side in 0..4 {
                    let p: HashSet<_> = perimeters
                        .iter()
                        .filter(|p| p.side == side)
                        .map(|p| (p.x, p.y))
                        .collect();
                    let sides = p.iter().map(|(x, y)| {
                        neighbors(&farm, *x, *y)
                            .iter()
                            .filter(|neighbor| match neighbor {
                                Some((nx, ny)) => !p.contains(&(*nx, *ny)),
                                None => true,
                            })
                            .count()
                            - 2
                    });
                    total_sides += sides.sum::<usize>() / 2;
                }
                result += area * total_sides;
            }
        }
    }
    result
}

struct Fence {
    pub x: usize,
    pub y: usize,
    pub side: usize,
}

fn walk2(
    farm: &Vec<Vec<char>>,
    (x, y): (usize, usize),
    visited: &mut Vec<Vec<bool>>,
) -> (usize, Vec<Fence>) {
    let plot = farm[y][x];
    visited[y][x] = true;
    let mut area = 1;
    let mut perimeters = vec![];
    for (side, n) in neighbors(farm, x, y).iter().enumerate() {
        if let Some((nx, ny)) = *n {
            if farm[ny][nx] == plot {
                if !visited[ny][nx] {
                    let (a, mut p) = walk2(farm, (nx, ny), visited);
                    area += a;
                    perimeters.append(&mut p);
                }
            } else {
                perimeters.push(Fence { x, y, side });
            }
        } else {
            perimeters.push(Fence { x, y, side });
        }
    }
    (area, perimeters)
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
