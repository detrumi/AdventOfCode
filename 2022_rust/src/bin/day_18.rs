use std::collections::{HashSet, VecDeque};

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_18.txt");

fn neighbors(pos: &Vec<isize>) -> Vec<Vec<isize>> {
    (-1..=1_isize)
        .flat_map(move |dx| {
            (-1..=1_isize).flat_map(move |dy| {
                (-1..=1_isize).filter_map(move |dz| {
                    if dx.abs() + dy.abs() + dz.abs() == 1 {
                        Some(vec![pos[0] + dx, pos[1] + dy, pos[2] + dz])
                    } else {
                        None
                    }
                })
            })
        })
        .collect()
}

fn part1() -> usize {
    let mut points = HashSet::new();
    for line in INPUT.trim().lines() {
        let parts = line
            .split(',')
            .map(|s| s.parse::<isize>().unwrap())
            .collect_vec();
        points.insert(parts);
    }

    let mut result = 0;
    for point in &points {
        for neighbor in neighbors(&point) {
            if !points.contains(&neighbor) {
                result += 1;
            }
        }
    }

    result
}

fn is_inside(start: &Vec<isize>, points: &HashSet<Vec<isize>>) -> bool {
    let mut visited = HashSet::new();
    visited.insert(start.clone());
    let mut queue = VecDeque::new();
    const MAX_DIST: usize = 25;
    queue.push_back((start.clone(), 0));
    while let Some((pos, dist)) = queue.pop_front() {
        for neighbor in neighbors(&pos) {
            if !points.contains(&neighbor) && !visited.contains(&neighbor) {
                if dist + 1 >= MAX_DIST {
                    return false;
                }
                visited.insert(neighbor.clone());
                queue.push_back((neighbor, dist + 1));
            }
        }
    }
    true
}

fn part2() -> usize {
    let mut points = HashSet::new();
    for line in INPUT.trim().lines() {
        let parts = line
            .split(',')
            .map(|s| s.parse::<isize>().unwrap())
            .collect_vec();
        points.insert(parts);
    }

    let mut empty = HashSet::new();
    let mut result = 0;
    for point in &points {
        for neighbor in neighbors(&point) {
            if !points.contains(&neighbor) {
                empty.insert(neighbor);
            }
        }
    }

    let mut inside = HashSet::new();
    for p in &empty {
        if is_inside(p, &points) {
            inside.insert(p);
        }
    }

    for point in &points {
        for neighbor in neighbors(&point) {
            if !points.contains(&neighbor) && !inside.contains(&neighbor) {
                result += 1;
            }
        }
    }

    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
