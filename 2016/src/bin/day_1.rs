use std::collections::HashSet;
use std::fs::File;
use std::io::*;

#[derive(Copy, Clone, PartialEq, Ord, PartialOrd, Eq, Debug)]
enum Dir {
    North,
    East,
    South,
    West,
}

use crate::Dir::*;

impl Dir {
    fn rotate(self, c: char) -> Self {
        match self {
            North => if c == 'L' { West } else { East },
            East => if c == 'L' { North } else { South },
            South => if c == 'L' { East } else { West },
            West => if c == 'L' { South } else { North },
        }
    }
}

impl Default for Dir {
    fn default() -> Self {
        North
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
struct Pos {
    x: i32,
    y: i32,
    facing: Dir,
}

impl Pos {
    pub fn do_move(&mut self, distance: i32) {
        match self.facing {
            North => self.y += distance,
            East => self.x += distance,
            South => self.y -= distance,
            West => self.x -= distance,
        }
    }
}

fn main() {
    let mut file = File::open("input/day_1.txt").expect("Unable to open file");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("Unable to read file");

    let mut pos = Pos::default();
    let mut first_revisit = true;
    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    for order in input.trim().split(", ") {
        let turn = order.chars().next().unwrap();
        pos.facing = pos.facing.rotate(turn);
        let mut distance = order.get(1..).unwrap().parse::<i32>().unwrap();
        while distance > 0 {
            distance -= 1;
            pos.do_move(1);
            if first_revisit && visited.contains(&(pos.x, pos.y)) {
                first_revisit = false;
                println!("Visited: {}", pos.x.abs() + pos.y.abs());
            }
            visited.insert((pos.x, pos.y));
        }
        pos.do_move(distance);
    }
    println!("Distance: {}", pos.x.abs() + pos.y.abs())
}
