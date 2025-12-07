use std::collections::{HashMap, HashSet};

const INPUT: &str = include_str!("../../input/day_7.txt");

#[derive(Clone, Copy, PartialEq, Eq, Default, Hash, Debug)]
struct Pos {
    pub x: usize,
    pub y: usize,
}

fn parse() -> (Vec<Vec<bool>>, Pos) {
    let mut start = Pos::default();
    let tiles = INPUT
        .trim()
        .lines()
        .enumerate()
        .map(|(y, line)| {
            line.chars()
                .enumerate()
                .map(|(x, c)| {
                    if c == 'S' {
                        start = Pos { x, y };
                    }
                    c == '^'
                })
                .collect()
        })
        .collect();
    (tiles, start)
}

fn part1() -> (Vec<Vec<bool>>, HashSet<Pos>) {
    let (tiles, Pos { x, mut y }) = parse();
    let mut xs = HashSet::new();
    xs.insert(x);

    let mut splits = HashSet::new();
    while y < tiles.len() - 1 {
        y += 1;
        let mut new_xs = HashSet::new();
        for x in xs {
            if tiles[y][x] {
                splits.insert(Pos { x, y });
                new_xs.insert(x - 1);
                new_xs.insert(x + 1);
            } else {
                new_xs.insert(x);
            }
        }
        xs = new_xs;
    }
    (tiles, splits)
}

fn part2(tiles: Vec<Vec<bool>>, splits: HashSet<Pos>) -> usize {
    let mut timelines = HashMap::new();
    for y in (0..tiles.len() - 1).rev() {
        for split in splits.iter().filter(|pos| pos.y == y) {
            let below = [-1, 1]
                .iter()
                .map(|dx| {
                    let x = (split.x as isize + dx) as usize;
                    (y + 1..tiles.len())
                        .filter_map(|y| timelines.get(&Pos { x, y }))
                        .next()
                        .copied()
                        .unwrap_or(1)
                })
                .sum::<usize>();
            timelines.insert(split, below);
        }
    }
    *timelines.iter().find(|(pos, _)| pos.y == 2).unwrap().1
}

fn main() {
    let (tiles, splits) = part1();
    println!("Part 1: {}", splits.len());
    println!("Part 2: {}", part2(tiles, splits));
}
