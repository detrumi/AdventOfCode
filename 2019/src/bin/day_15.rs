use rand::Rng;
use std::collections::{HashMap, HashSet, VecDeque};
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

    pub fn go_dir(self, dir: Dir) -> Self {
        match dir {
            Dir::North => Pos::new(self.x, self.y - 1),
            Dir::East => Pos::new(self.x + 1, self.y),
            Dir::South => Pos::new(self.x, self.y + 1),
            Dir::West => Pos::new(self.x - 1, self.y),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Dir {
    North,
    East,
    South,
    West,
}

impl Default for Dir {
    fn default() -> Self {
        Dir::North
    }
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

    pub fn to_command(self) -> i32 {
        match self {
            Dir::North => 1,
            Dir::East => 4,
            Dir::South => 2,
            Dir::West => 3,
        }
    }

    pub fn parse(n: i32) -> Self {
        match n {
            1 => Dir::North,
            4 => Dir::East,
            2 => Dir::South,
            3 => Dir::West,
            _ => panic!(),
        }
    }
}

fn main() {
    let file = File::open("input/day_15.txt").unwrap();
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

    let mut program = Program::new(mem.clone());
    let oxygen_system_pos = program.calculate();

    let mut visited: HashSet<Pos> = HashSet::new();
    visited.insert(oxygen_system_pos);
    let mut queue: VecDeque<(i32, Pos)> = VecDeque::new();
    queue.push_back((0, oxygen_system_pos));

    let mut max_dist = 0;
    while let Some((dist, pos)) = queue.pop_front() {
        for dir in &vec![Dir::North, Dir::East, Dir::South, Dir::West] {
            let new_pos = pos.go_dir(*dir);
            if !*program.walls.entry(new_pos).or_default() && !visited.contains(&new_pos) {
                visited.insert(new_pos);
                queue.push_back((dist + 1, new_pos));
                max_dist = max_dist.max(dist + 1);

                if new_pos == Pos::default() {
                    println!("Part 1 = {:?}", dist + 1);
                }
            }
        }
    }

    println!("Part 2 = {:?}", max_dist);
}

#[derive(Default)]
struct Program {
    i: usize,
    instruction: i64,
    relative_base: i64,
    mem: Vec<i64>,
    pos: Pos,
    dir: Dir,
    walls: HashMap<Pos, bool>,
}

impl Program {
    fn new(mem: Vec<i64>) -> Self {
        let mut program = Program::default();
        program.mem = mem;
        program.walls.insert(Pos::new(0, 0), false);
        program
    }

    fn calculate(&mut self) -> Pos {
        let mut result = Pos::default();
        let mut step = 0;
        while let Some(instruction) = self.mem.get(self.i) {
            step += 1;
            if step > 100_000_000 {
                break;
            }
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
                    let mut rng = rand::thread_rng();
                    let dir = rng.gen_range(1, 5);
                    self.dir = Dir::parse(dir);
                    self.write(1, dir as i64);
                    self.i += 2;
                }
                4 => {
                    let output = self.param(1);
                    let new_pos = self.pos.go_dir(self.dir);
                    match output {
                        0 => {
                            self.walls.insert(new_pos, true);
                        }
                        1 => {
                            self.walls.insert(new_pos, false);
                            self.pos = new_pos;
                        }
                        2 => {
                            result = new_pos;
                            self.walls.insert(new_pos, false);
                            self.pos = new_pos;
                        }
                        _ => panic!(),
                    }
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
                99 => break,
                n => panic!(format!("{}", n)),
            }
        }

        for y in self.walls.keys().map(|p| p.y).min().unwrap()
            ..=self.walls.keys().map(|p| p.y).max().unwrap()
        {
            for x in self.walls.keys().map(|p| p.x).min().unwrap()
                ..=self.walls.keys().map(|p| p.x).max().unwrap()
            {
                let pos = Pos::new(x, y);
                let tile = if pos == result || pos == Pos::default() {
                    'X'
                } else if !self.walls.contains_key(&pos) {
                    ' '
                } else if self.walls[&pos] {
                    '#'
                } else {
                    '.'
                };
                print!("{}", tile);
            }
            println!();
        }

        result
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
        self.mem[self.target(index)]
    }

    fn write(&mut self, index: usize, value: i64) {
        let target = self.target(index);
        if target >= self.mem.len() {
            self.mem.resize(target + 1, 0);
        }
        self.mem[target] = value;
    }
}
