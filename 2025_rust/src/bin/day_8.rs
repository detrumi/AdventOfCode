use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_8.txt");

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash, Debug)]
struct Pos {
    pub x: usize,
    pub y: usize,
    pub z: usize,
}

impl Pos {
    pub fn distance_to(&self, other: &Pos) -> usize {
        self.x.abs_diff(other.x).pow(2)
            + self.y.abs_diff(other.y).pow(2)
            + self.z.abs_diff(other.z).pow(2)
    }
}

fn parse() -> Vec<Pos> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let parts = line
                .split_terminator(",")
                .map(|n| n.parse().unwrap())
                .collect_vec();
            Pos {
                x: parts[0],
                y: parts[1],
                z: parts[2],
            }
        })
        .collect()
}

fn part1() -> usize {
    let mut input = parse()
        .into_iter()
        .enumerate()
        .map(|(group, n)| (group, n, false))
        .collect_vec();
    let mut min_dist = 0;
    for _ in 0..1000 {
        let mut min = (0, 0, usize::MAX);
        for i in 0..input.len() - 1 {
            for j in i + 1..input.len() {
                let dist = input[i].1.distance_to(&input[j].1);
                if dist > min_dist && dist < min.2 {
                    min = (i, j, dist);
                }
            }
        }
        if min.2 == usize::MAX {
            break;
        }
        let (i, j, dist) = min;
        min_dist = dist;
        if input[i].0 == input[j].0 {
            continue;
        }

        input[i].2 = true;
        input[j].2 = true;
        let from = input[i].0;
        let to = input[j].0;
        for i in 0..input.len() {
            if input[i].0 == from {
                input[i].0 = to;
            }
        }
    }
    input
        .iter()
        .map(|t| t.0)
        .counts()
        .into_iter()
        .map(|t| t.1)
        .sorted()
        .rev()
        .take(3)
        .product()
}

fn part2() -> usize {
    let mut input = parse()
        .into_iter()
        .enumerate()
        .map(|(group, n)| (group, n, false))
        .collect_vec();
    let mut min_dist = 0;
    let mut product = 0;
    loop {
        let mut min = (0, 0, usize::MAX);
        for i in 0..input.len() - 1 {
            for j in i + 1..input.len() {
                if input[i].2 && input[j].2 || input[i].0 == input[j].0 {
                    continue;
                }
                let dist = input[i].1.distance_to(&input[j].1);
                if dist > min_dist && dist < min.2 {
                    min = (i, j, dist);
                }
            }
        }
        if min.2 == usize::MAX {
            break;
        }
        let (i, j, dist) = min;
        min_dist = dist;
        input[i].2 = true;
        input[j].2 = true;
        product = input[i].1.x * input[j].1.x;
        let from = input[i].0;
        let to = input[j].0;
        for i in 0..input.len() {
            if input[i].0 == from {
                input[i].0 = to;
            }
        }
        let first_group = input[0].0;
        if input.iter().all(|t| t.0 == first_group) {
            break;
        }
    }
    product
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
