use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Default)]
struct Pos {
    x: i32,
    y: i32,
}

impl Pos {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    pub fn parse(s: &str) -> Self {
        let dist = s[1..].parse::<i32>().unwrap();
        match s.chars().next().unwrap() {
            'R' => Self::new(dist, 0),
            'L' => Self::new(-dist, 0),
            'U' => Self::new(0, dist),
            'D' => Self::new(0, -dist),
            _ => panic!(),
        }
    }

    pub fn distance(self) -> i32 {
        self.x.abs() + self.y.abs()
    }

    pub fn step(&mut self, target: &mut Pos) {
        if self.x > 0 {
            self.x -= 1;
            target.x += 1;
        } else if self.x < 0 {
            self.x += 1;
            target.x -= 1;
        } else if self.y > 0 {
            self.y -= 1;
            target.y += 1;
        } else {
            self.y += 1;
            target.y -= 1;
        }
    }
}

fn main() {
    let file = File::open("input/day_3.txt").unwrap();
    let lines = io::BufReader::new(file).lines().map(|l| {
        l.unwrap()
            .split(',')
            .map(|s| Pos::parse(s))
            .collect::<Vec<Pos>>()
    });

    let mut visited: HashMap<Pos, i32> = HashMap::new();
    let mut is_first_wire = true;
    let mut closest = Pos::new(std::i32::MAX / 2, std::i32::MAX / 2);
    let mut least_delay_pos = Pos::new(std::i32::MAX / 2, std::i32::MAX / 2);
    let mut least_delay = std::i32::MAX / 2;
    for line in lines {
        let mut pos = Pos::default();
        let mut delay = 0;
        for mut wire in line {
            while wire.distance() > 0 {
                wire.step(&mut pos);
                delay += 1;
                if is_first_wire {
                    visited.insert(pos, delay);
                }
                if !is_first_wire && visited.contains_key(&pos) {
                    if pos.distance() < closest.distance() {
                        closest = pos;
                    }
                    if visited[&pos] + delay < least_delay {
                        least_delay = visited[&pos] + delay;
                        least_delay_pos = pos;
                    }
                }
            }
        }
        is_first_wire = false;
    }
    eprintln!("closest = {:?}", closest);
    eprintln!("closest.distance() = {:?}", closest.distance());
    eprintln!("least_delay_pos = {:?}", least_delay_pos);
    eprintln!("least_delay = {:?}", least_delay);
}
