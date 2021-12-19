use std::{
    collections::HashSet,
    ops::{Add, Sub},
};

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_19.txt");

#[derive(Clone, Copy, PartialEq, Eq, Default, Hash, Debug)]
struct Pos {
    pub x: isize,
    pub y: isize,
    pub z: isize,
}

impl Add for Pos {
    type Output = Pos;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.x + rhs.x, self.y + rhs.y, self.z + rhs.z)
    }
}

impl Sub for Pos {
    type Output = Pos;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.x - rhs.x, self.y - rhs.y, self.z - rhs.z)
    }
}

fn parse() -> Vec<Vec<Vec<Pos>>> {
    INPUT
        .trim()
        .split("\n\n")
        .map(|part| {
            let mut scanner = vec![];
            for line in part.lines().skip(1) {
                let (x, y, z) = line
                    .split(',')
                    .map(|s| s.parse::<isize>().unwrap())
                    .collect_tuple()
                    .unwrap();
                let pos = Pos::new(x, y, z);
                scanner.push(pos);
            }
            rotations(scanner)
        })
        .collect()
}

impl Pos {
    pub fn new(x: isize, y: isize, z: isize) -> Self {
        Self { x, y, z }
    }

    fn rotate_x(self) -> Self {
        Self::new(self.x, self.z, -self.y)
    }

    fn rotate_y(self) -> Self {
        Self::new(self.z, self.y, -self.x)
    }

    fn rotate_z(self) -> Self {
        Self::new(self.y, -self.x, self.z)
    }

    fn manhattan_distance(left: Pos, right: Pos) -> isize {
        (right.x - left.x).abs() + (right.y - left.y).abs() + (right.z - left.z).abs()
    }
}

fn rotations(mut v: Vec<Pos>) -> Vec<Vec<Pos>> {
    let mut result = HashSet::new();
    for _ in 0..4 {
        v = v.into_iter().map(|pos| pos.rotate_x()).collect();
        result.insert(v.clone());
        let mut v2 = v.clone();
        for _ in 0..4 {
            v2 = v2.into_iter().map(|pos| pos.rotate_y()).collect();
            result.insert(v2.clone());
            let mut v3 = v2.clone();
            for _ in 0..4 {
                v3 = v3.into_iter().map(|pos| pos.rotate_z()).collect();
                result.insert(v3.clone());
            }
        }
    }
    result.into_iter().collect()
}

fn solve() -> (usize, isize) {
    let mut input = parse();
    let mut lefts: HashSet<Pos> = input.remove(0)[0].iter().cloned().collect();
    let mut scanners: HashSet<Pos> = HashSet::new();
    scanners.insert(Pos::default());
    while !input.is_empty() {
        let mut right_index = 0;
        'outer: while right_index < input.len() {
            for i in 0..input[right_index].len() {
                let right = &input[right_index][i];
                for left_pos in &lefts {
                    for right_pos in right {
                        let delta = *left_pos - *right_pos;
                        let mut intersections = 0;
                        for pos in right {
                            if lefts.contains(&(*pos + delta)) {
                                intersections += 1;
                                if intersections >= 12 {
                                    lefts.extend(right.into_iter().map(|p| *p + delta));
                                    scanners.insert(delta);
                                    input.remove(right_index);
                                    continue 'outer;
                                }
                            }
                        }
                    }
                }
            }
            right_index += 1;
        }
    }
    let max_distance = scanners
        .iter()
        .permutations(2)
        .map(|v| Pos::manhattan_distance(*v[0], *v[1]))
        .max()
        .unwrap();
    (lefts.len(), max_distance)
}

fn main() {
    let (beacon_count, max_distance) = solve();
    println!("Part 1: {}", beacon_count);
    println!("Part 2: {}", max_distance);
}
