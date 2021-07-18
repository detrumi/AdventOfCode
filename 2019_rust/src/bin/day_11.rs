use std::collections::HashSet;
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
    let file = File::open("input/day_11.txt").unwrap();
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
    println!("Part 1 = {:?}", part_1.painted.len());

    let mut part_2 = Program::new(mem.clone(), true);
    part_2.calculate();
    let wp = part_2.white_panels.clone();
    for y in wp.iter().map(|p| p.y).min().unwrap()..=wp.iter().map(|p| p.y).max().unwrap() {
        for x in wp.iter().map(|p| p.x).min().unwrap()..=wp.iter().map(|p| p.x).max().unwrap() {
            print!(
                "{}",
                if wp.contains(&Pos::new(x, y)) {
                    '#'
                } else {
                    '.'
                }
            )
        }
        println!();
    }
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
            Dir::North => Dir::West,
            Dir::East => Dir::North,
            Dir::South => Dir::East,
            Dir::West => Dir::South,
        }
    }

    pub fn right(self) -> Self {
        match self {
            Dir::North => Dir::East,
            Dir::East => Dir::South,
            Dir::South => Dir::West,
            Dir::West => Dir::North,
        }
    }
}
struct Program {
    i: usize,
    instruction: i64,
    relative_base: i64,
    mem: Vec<i64>,
    pub painted: HashSet<Pos>,
    pub white_panels: HashSet<Pos>,
    pos: Pos,
    dir: Dir,
}

impl Program {
    fn new(mem: Vec<i64>, starts_on_white: bool) -> Self {
        let mut program = Self {
            i: 0,
            instruction: 0,
            relative_base: 0,
            mem,
            painted: HashSet::new(),
            white_panels: HashSet::new(),
            pos: Pos::new(0, 0),
            dir: Dir::North,
        };
        if starts_on_white {
            program.white_panels.insert(program.pos);
            program.painted.insert(program.pos);
        }
        program
    }

    fn calculate(&mut self) {
        let mut expects_color = true;
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
                    self.write(1, self.white_panels.contains(&self.pos) as i64);
                    self.i += 2;
                }
                4 => {
                    let output = self.param(1);
                    if expects_color {
                        self.painted.insert(self.pos);
                        match output {
                            0 => {
                                self.white_panels.remove(&self.pos);
                            }
                            1 => {
                                self.white_panels.insert(self.pos);
                            }
                            n => panic!(format!("{}", n)),
                        }
                    } else {
                        match output {
                            0 => self.dir = self.dir.left(),
                            1 => self.dir = self.dir.right(),
                            n => panic!(format!("{}", n)),
                        }
                        match self.dir {
                            Dir::North => self.pos = Pos::new(self.pos.x, self.pos.y - 1),
                            Dir::East => self.pos = Pos::new(self.pos.x + 1, self.pos.y),
                            Dir::South => self.pos = Pos::new(self.pos.x, self.pos.y + 1),
                            Dir::West => self.pos = Pos::new(self.pos.x - 1, self.pos.y),
                        }
                    }
                    expects_color = !expects_color;
                    self.painted.insert(self.pos);
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
            2 => self.relative_base as usize + self.mem[self.i + index as usize] as usize,
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
