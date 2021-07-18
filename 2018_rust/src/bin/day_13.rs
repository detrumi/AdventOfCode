use core::cmp::Ordering;
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Default)]
pub struct Pos {
    x: i32,
    y: i32,
}

impl Pos {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    pub fn step(self, dir: Dir) -> Self {
        match dir {
            North => Pos::new(self.x, self.y - 1),
            East => Pos::new(self.x + 1, self.y),
            South => Pos::new(self.x, self.y + 1),
            West => Pos::new(self.x - 1, self.y),
        }
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Pos) -> Ordering {
        self.y.cmp(&other.y).then(self.x.cmp(&other.x))
    }
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Pos) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub enum Track {
    Horizontal,
    Vertical,
    CurveNE,
    CurveNW,
    Intersection,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Dir {
    North,
    East,
    South,
    West,
}

impl Dir {
    pub fn left(self) -> Self {
        match self {
            North => West,
            East => North,
            South => East,
            West => South,
        }
    }

    pub fn right(self) -> Self {
        match self {
            North => East,
            East => South,
            South => West,
            West => North,
        }
    }

    pub fn is_horizontal(self) -> bool {
        self == West || self == East
    }

    pub fn is_vertical(self) -> bool {
        self == North || self == South
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Stage {
    GoLeft,
    GoStraight,
    GoRight,
}

impl Stage {
    pub fn next(&mut self) {
        *self = match *self {
            GoLeft => GoStraight,
            GoStraight => GoRight,
            GoRight => GoLeft,
        }
    }

    pub fn turn(&mut self, dir: Dir) -> Dir {
        let result = match *self {
            GoLeft => dir.left(),
            GoStraight => dir,
            GoRight => dir.right(),
        };
        self.next();
        result
    }
}

use crate::Dir::*;
use crate::Stage::*;
use crate::Track::*;

impl Track {
    fn parse(c: char) -> Option<(Track, Option<Dir>)> {
        if c == ' ' {
            return None;
        };
        Some(match c {
            '-' => (Horizontal, None),
            '<' => (Horizontal, Some(West)),
            '>' => (Horizontal, Some(East)),
            '|' => (Vertical, None),
            '^' => (Horizontal, Some(North)),
            'v' => (Horizontal, Some(South)),
            '/' => (CurveNE, None),
            '\\' => (CurveNW, None),
            '+' => (Intersection, None),
            _ => panic!(format!("Invalid track piece: {}", c)),
        })
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Cart {
    pos: Pos,
    dir: Dir,
    stage: Stage,
}

impl Cart {
    pub fn do_move(&mut self, tracks: &HashMap<Pos, Track>) {
        self.pos = self.pos.step(self.dir);
        self.dir = match tracks
            .get(&self.pos)
            .expect(&format!("No tracks at {:?}!", self.pos))
        {
            Horizontal | Vertical => self.dir,
            CurveNE if self.dir.is_vertical() => self.dir.right(),
            CurveNE => self.dir.left(),
            CurveNW if self.dir.is_vertical() => self.dir.left(),
            CurveNW => self.dir.right(),
            Intersection => self.stage.turn(self.dir),
        };
    }
}

fn main() {
    let file = File::open("input/day_13.txt").unwrap();
    let lines = io::BufReader::new(file).lines().map(|l| l.unwrap());

    let mut tracks: HashMap<Pos, Track> = HashMap::new();
    let mut carts: Vec<Cart> = vec![];
    for (y, line) in lines.enumerate() {
        for (x, c) in line.chars().enumerate() {
            if let Some((track, cart)) = Track::parse(c) {
                let pos = Pos::new(x as i32, y as i32);
                tracks.insert(pos, track);
                if let Some(dir) = cart {
                    carts.push(Cart {
                        pos,
                        dir,
                        stage: GoLeft,
                    });
                }
            }
        }
    }

    let mut crashed = false;
    loop {
        carts.sort_by_key(|cart| cart.pos);
        let mut i = 0;
        while i < carts.len() {
            carts[i].do_move(&tracks);
            let pos = carts[i].pos;
            if let Some(crasher) = (0..carts.len()).position(|c| c != i && carts[c].pos == pos) {
                if !crashed {
                    println!("Part 1: {},{}", pos.x, pos.y);
                    crashed = true;
                }
                if crasher < i {
                    carts.remove(i);
                    carts.remove(crasher);
                    i -= 1;
                } else {
                    carts.remove(crasher);
                    carts.remove(i);
                }
            } else {
                i += 1;
            }
        }
        if carts.len() == 1 {
            println!("Part 2: {},{}", carts[0].pos.x, carts[0].pos.y);
            return;
        }
    }
}
