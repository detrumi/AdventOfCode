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
}

fn main() {
    let file = File::open("input/day_13.txt").unwrap();
    let mem: Vec<i64> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            l.unwrap()
                .split(',')
                .map(|s| s.parse::<i64>().unwrap())
                .collect()
        })
        .next()
        .unwrap();

    let mut part_1 = Program::new(mem.clone(), false);
    part_1.calculate();
    println!(
        "Part 1 = {:?}",
        part_1
            .tiles
            .iter()
            .filter(|(_, t)| **t == Tile::Block)
            .count()
    );

    let mut part_2 = Program::new(mem.clone(), true);
    part_2.calculate();
    println!("Part 2 = {:?}", part_2.score);
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

impl Tile {
    pub fn parse(n: i64) -> Self {
        match n {
            0 => Tile::Empty,
            1 => Tile::Wall,
            2 => Tile::Block,
            3 => Tile::Paddle,
            4 => Tile::Ball,
            _ => panic!(),
        }
    }
}

#[derive(Default)]
struct Program {
    i: usize,
    instruction: i64,
    relative_base: i64,
    mem: Vec<i64>,
    pos: Pos,
    pub tiles: HashMap<Pos, Tile>,
    output_kind: i32,
    score: i64,
    ball_pos: i32,
    paddle_pos: i32,
}

impl Program {
    fn new(mut mem: Vec<i64>, play_game: bool) -> Self {
        if play_game {
            mem[0] = 2;
        }
        let mut program = Self::default();
        program.mem = mem;
        program
    }

    fn calculate(&mut self) {
        while let Some(instruction) = self.mem.get(self.i) {
            self.instruction = *instruction;

            match self.instruction % 100 {
                1 => {
                    let value = self.param(1) + self.param(2);
                    self.write(3, value);
                    self.i += 4;
                }
                2 => {
                    let value = self.param(1) * self.param(2);
                    self.write(3, value);
                    self.i += 4;
                }
                3 => {
                    let input = if self.ball_pos == self.paddle_pos {
                        0
                    } else if self.ball_pos < self.paddle_pos {
                        -1
                    } else {
                        1
                    };
                    self.write(1, input);
                    self.i += 2;
                }
                4 => {
                    let output = self.param(1);
                    match self.output_kind {
                        0 => {
                            self.pos.x = output as i32;
                        }
                        1 => {
                            self.pos.y = output as i32;
                        }
                        2 => {
                            if self.pos == Pos::new(-1, 0) {
                                self.score = output;
                            } else {
                                let tile = Tile::parse(output);
                                if tile == Tile::Empty {
                                    self.tiles.remove(&self.pos);
                                } else {
                                    self.tiles.insert(self.pos, tile);
                                }
                                if tile == Tile::Ball {
                                    self.ball_pos = self.pos.x;
                                } else if tile == Tile::Paddle {
                                    self.paddle_pos = self.pos.x;
                                }
                                self.pos = Pos::default();
                            }
                        }
                        _ => panic!(),
                    }
                    self.output_kind = (self.output_kind + 1) % 3;
                    self.i += 2;
                }
                5 => {
                    self.i = if self.param(1) != 0 {
                        self.param(2) as usize
                    } else {
                        self.i + 3
                    }
                }
                6 => {
                    self.i = if self.param(1) == 0 {
                        self.param(2) as usize
                    } else {
                        self.i + 3
                    };
                }
                7 => {
                    let value = (self.param(1) < self.param(2)) as i64;
                    self.write(3, value);
                    self.i += 4;
                }
                8 => {
                    let value = (self.param(1) == self.param(2)) as i64;
                    self.write(3, value);
                    self.i += 4;
                }
                9 => {
                    self.relative_base += self.param(1);
                    self.i += 2;
                }
                99 => return,
                n => panic!(format!("{}", n)),
            }
        }
    }

    fn target(&self, index: usize) -> usize {
        match self.mode(index) {
            0 => self.mem[self.i + index] as usize,
            1 => self.i + index,
            2 => (self.relative_base + self.mem[self.i + index as usize]) as usize,
            _ => panic!(),
        }
    }

    fn mode(&self, index: usize) -> i64 {
        match index {
            1 => (self.instruction / 100) % 10,
            2 => (self.instruction / 1_000) % 10,
            3 => (self.instruction / 10_000) % 10,
            _ => panic!(),
        }
    }

    fn param(&mut self, index: usize) -> i64 {
        let target = self.target(index);
        if target >= self.mem.len() {
            self.mem.resize(target + 1, 0);
        }
        self.mem[target]
    }

    fn write(&mut self, index: usize, value: i64) {
        let target = self.target(index);
        if target >= self.mem.len() {
            self.mem.resize(target + 1, 0);
        }
        self.mem[target] = value;
    }
}
