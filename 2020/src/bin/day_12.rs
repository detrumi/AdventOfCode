const INPUT: &str = include_str!("../../input/day_12.txt");

#[derive(Clone, Copy, Debug, Default)]
struct Pos {
    pub x: i64,
    pub y: i64,
}

impl Pos {
    pub fn rotate_left_around_origin(self) -> Self {
        Pos {
            x: -self.y,
            y: self.x,
        }
    }
}

#[derive(Debug)]
enum Dir {
    North,
    East,
    South,
    West,
}

impl Dir {
    pub fn rotate(self, c: char) -> Self {
        let left = c == 'L';
        match self {
            North => {
                if left {
                    West
                } else {
                    East
                }
            }
            East => {
                if left {
                    North
                } else {
                    South
                }
            }
            South => {
                if left {
                    East
                } else {
                    West
                }
            }
            West => {
                if left {
                    South
                } else {
                    North
                }
            }
        }
    }
}

use Dir::*;

fn part1() -> i64 {
    let mut pos = Pos::default();
    let mut dir = East;
    for line in INPUT.lines() {
        let c = line.chars().next().unwrap();
        let n = line[1..].parse::<i64>().unwrap();
        match c {
            'N' => pos.y -= n,
            'S' => pos.y += n,
            'E' => pos.x += n,
            'W' => pos.x -= n,
            'L' | 'R' => {
                for _ in 0..n / 90 {
                    dir = dir.rotate(c);
                }
            }
            'F' => match dir {
                North => pos.y -= n,
                South => pos.y += n,
                East => pos.x += n,
                West => pos.x -= n,
            },
            _ => panic!(),
        }
    }
    pos.x.abs() + pos.y.abs()
}

fn part2() -> i64 {
    let mut pos = Pos::default();
    let mut waypoint = Pos { x: 10, y: -1 };
    for line in INPUT.lines() {
        let c = line.chars().next().unwrap();
        let n = line[1..].parse::<i64>().unwrap();
        match c {
            'N' => waypoint.y -= n,
            'S' => waypoint.y += n,
            'E' => waypoint.x += n,
            'W' => waypoint.x -= n,
            'L' => {
                for _ in 0..(360 - n) / 90 {
                    waypoint = waypoint.rotate_left_around_origin();
                }
            }
            'R' => {
                for _ in 0..n / 90 {
                    waypoint = waypoint.rotate_left_around_origin();
                }
            }
            'F' => {
                pos = Pos {
                    x: pos.x + n * waypoint.x,
                    y: pos.y + n * waypoint.y,
                }
            }
            _ => panic!(),
        }
    }
    pos.x.abs() + pos.y.abs()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
