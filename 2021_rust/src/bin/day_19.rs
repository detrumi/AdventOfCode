use std::collections::HashSet;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_19.txt");

#[derive(Clone, Copy, PartialEq, Eq, Default, Hash, Debug)]
struct Pos {
    pub x: isize,
    pub y: isize,
    pub z: isize,
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
    let mut scanners = parse();
    let initial_left = scanners.remove(0)[0].iter().cloned().collect();
    let mut lefts: Vec<(Pos, HashSet<Pos>)> = vec![(Pos::default(), initial_left)];
    while !scanners.is_empty() {
        let mut right_index = 0;
        'outer: while right_index < scanners.len() {
            for i in 0..scanners[right_index].len() {
                let rotation = &scanners[right_index][i];
                for (_, left) in &lefts {
                    for left_pos in left {
                        for right_pos in rotation {
                            let delta = Pos::new(
                                left_pos.x - right_pos.x,
                                left_pos.y - right_pos.y,
                                left_pos.z - right_pos.z,
                            );
                            let right: HashSet<Pos> = rotation
                                .into_iter()
                                .map(|p| Pos::new(p.x + delta.x, p.y + delta.y, p.z + delta.z))
                                .collect();
                            if left.intersection(&right).count() >= 12 {
                                lefts.push((delta, right));
                                scanners.remove(right_index);
                                continue 'outer;
                            }
                        }
                    }
                }
            }
            right_index += 1;
        }
    }
    let beacons: HashSet<_> = lefts.iter().flat_map(|(_, set)| set.iter()).collect();
    let max_distance = lefts
        .iter()
        .map(|(pos, _)| pos)
        .permutations(2)
        .map(|v| Pos::manhattan_distance(*v[0], *v[1]))
        .max()
        .unwrap();
    (beacons.len(), max_distance)
}

fn main() {
    let (beacon_count, max_distance) = solve();
    println!("Part 1: {}", beacon_count);
    println!("Part 2: {}", max_distance);
}
