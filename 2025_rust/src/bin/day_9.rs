use std::ops::RangeInclusive;

use fx_hash::{FxHashSet, FxHashSetExt};
use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_9.txt");

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash, Debug)]
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

fn part2() -> usize {
    let input = parse();
    let xs = input.iter().map(|p| p.x).sorted().dedup().collect_vec();
    let ys = input.iter().map(|p| p.y).sorted().dedup().collect_vec();
    let shrunk = input
        .iter()
        .map(|pos| Pos {
            x: xs.iter().position(|&x| x == pos.x).unwrap(),
            y: ys.iter().position(|&y| y == pos.y).unwrap(),
        })
        .collect_vec();

    let lines = shrunk
        .iter()
        .chain(std::iter::once(&shrunk[0]))
        .copied()
        .tuple_windows::<(Pos, Pos)>();

    let mut fill = FxHashSet::new();
    for (from, to) in lines {
        let dx = (to.x as i32 - from.x as i32).signum();
        let dy = (to.y as i32 - from.y as i32).signum();
        let (mut x, mut y) = (from.x as i32, from.y as i32);
        while (x, y) != (to.x as i32, to.y as i32) {
            fill.insert(Pos {
                x: x as usize,
                y: y as usize,
            });
            x += dx;
            y += dy;
        }
        fill.insert(Pos {
            x: x as usize,
            y: y as usize,
        });
    }

    let dx = (shrunk[1].x as i32 - shrunk[0].x as i32).signum()
        + (shrunk.last().unwrap().x as i32 - shrunk[0].x as i32).signum();
    let dy = (shrunk[1].y as i32 - shrunk[0].y as i32).signum()
        + (shrunk.last().unwrap().y as i32 - shrunk[0].y as i32).signum();
    let start = Pos {
        x: (shrunk[0].x as i32 + dx) as usize,
        y: (shrunk[0].y as i32 + dy) as usize,
    };
    let mut queue = vec![start];
    fill.insert(start);
    while let Some(pos) = queue.pop() {
        for (dx, dy) in [(1, 0), (0, 1), (-1, 0), (0, -1)] {
            let neighbor = Pos {
                x: (pos.x as i32 + dx) as usize,
                y: (pos.y as i32 + dy) as usize,
            };
            if fill.insert(neighbor) {
                queue.push(neighbor);
            }
        }
    }

    let mut best = 0;
    for i in 0..input.len() - 1 {
        'outer: for j in i + 1..input.len() {
            let area =
                (input[i].x.abs_diff(input[j].x) + 1) * (input[i].y.abs_diff(input[j].y) + 1);
            if area > best {
                for x in range(shrunk[i].x, shrunk[j].x) {
                    for y in range(shrunk[i].y, shrunk[j].y) {
                        if !fill.contains(&Pos { x, y }) {
                            continue 'outer;
                        }
                    }
                }
                best = area;
            }
        }
    }
    best
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
