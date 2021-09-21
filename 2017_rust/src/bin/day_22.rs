use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

const INPUT: &str = include_str!("../../input/day_22.txt");

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct Pos {
    x: i32,
    y: i32,
}

impl Pos {
    pub fn new(x: i32, y: i32) -> Self {
        Pos { x, y }
    }

    pub fn forward(self, dir: Dir) -> Self {
        match dir {
            Dir::North => Pos::new(self.x, self.y - 1),
            Dir::East => Pos::new(self.x + 1, self.y),
            Dir::South => Pos::new(self.x, self.y + 1),
            Dir::West => Pos::new(self.x - 1, self.y),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Dir {
    North,
    East,
    South,
    West,
}

impl Dir {
    pub fn turn_right(self, steps: u32) -> Self {
        match (self as u32 + steps) % 4 {
            0 => Self::North,
            1 => Self::East,
            2 => Self::South,
            3 => Self::West,
            _ => unreachable!(),
        }
    }
}

fn parse() -> (HashSet<Pos>, Pos) {
    let mut result = HashSet::new();
    let lines: Vec<_> = INPUT.trim().lines().collect();
    for (y, line) in lines.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                result.insert(Pos::new(x as i32, y as i32));
            }
        }
    }
    let size = lines.len() as i32;
    let center = Pos::new(size / 2, size / 2);
    (result, center)
}

fn part1() -> usize {
    let (mut map, mut pos) = parse();
    let mut dir = Dir::North;
    let mut infections = 0;
    for _ in 0..10_000 {
        let was_infected = map.contains(&pos);
        dir = dir.turn_right(if was_infected { 1 } else { 3 });
        if was_infected {
            map.remove(&pos);
        } else {
            map.insert(pos);
            infections += 1;
        }
        pos = pos.forward(dir);
    }
    infections
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum State {
    Weakened,
    Infected,
    Flagged,
}

fn part2() -> usize {
    let (map, mut pos) = parse();
    let mut map: HashMap<Pos, State> = map.into_iter().map(|p| (p, State::Infected)).collect();
    let mut dir = Dir::North;
    let mut infections = 0;
    for _ in 0..10_000_000 {
        let state = map.get(&pos);
        let (turn, new_state) = match state {
            None => (3, Some(State::Weakened)),
            Some(State::Weakened) => (0, Some(State::Infected)),
            Some(State::Infected) => (1, Some(State::Flagged)),
            Some(State::Flagged) => (2, None),
        };
        dir = dir.turn_right(turn);
        if let Some(new_state) = new_state {
            map.insert(pos, new_state);
            if new_state == State::Infected {
                infections += 1;
            }
        } else {
            map.remove(&pos);
        }
        pos = pos.forward(dir);
    }
    infections
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
