use std::fs::File;
use std::io::{self, BufRead};
use std::iter;
use std::{char, cmp};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Default)]
struct Pos {
    x: usize,
    y: usize,
}

impl Pos {
    pub fn new(x: usize, y: usize) -> Self {
        Self { x, y }
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

fn main() {
    let file = File::open("input/day_17.txt").unwrap();
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

    let part1 = Program::new(mem.clone(), 1)
        .calculate(vec![])
        .iter()
        .map(|i| char::from_u32(*i as u32).unwrap())
        .collect::<String>();

    println!("Part 1:\n{}", part1);
    let mut tiles = part1
        .trim()
        .lines()
        .map(|l| l.chars().chain(iter::once('.')).collect::<Vec<char>>())
        .collect::<Vec<_>>();
    tiles.push(iter::repeat('.').take(tiles[0].len()).collect());

    let mut paths_x = vec![];
    let mut pos: Pos = Pos::default();
    let mut dir = Dir::default();
    for y in 0..tiles.len() {
        let mut row_start: Option<usize> = None;
        for x in 0..tiles[0].len() {
            if tiles[y][x] == '.' {
                if let Some(row_start) = row_start {
                    if x - row_start > 1 {
                        paths_x.push((Pos::new(row_start, y), Pos::new(x - 1, y)));
                    }
                }
                row_start = None;
            } else {
                if row_start.is_none() {
                    row_start = Some(x);
                }
                if tiles[y][x] != '#' {
                    pos = Pos::new(x, y);
                    dir = match tiles[y][x] {
                        '^' => Dir::North,
                        '>' => Dir::East,
                        'v' => Dir::South,
                        '<' => Dir::West,
                        _ => panic!(),
                    };
                }
            }
        }
    }
    let mut paths_y = vec![];
    for x in 0..tiles[0].len() {
        let mut col_start: Option<usize> = None;
        for y in 0..tiles.len() {
            if tiles[y][x] == '.' {
                if let Some(col_start) = col_start {
                    if y - col_start > 1 {
                        paths_y.push((Pos::new(x, col_start), Pos::new(x, y - 1)));
                    }
                }
                col_start = None;
            } else {
                if col_start.is_none() {
                    col_start = Some(y);
                }
                if tiles[y][x] != '#' {
                    pos = Pos::new(x, y);
                    dir = match tiles[y][x] {
                        '^' => Dir::North,
                        '>' => Dir::East,
                        'v' => Dir::South,
                        '<' => Dir::West,
                        _ => panic!(),
                    };
                }
            }
        }
    }
    let mut alignment_sum = 0;
    for (left, right) in &paths_x {
        for (top, bottom) in &paths_y {
            if top.x > left.x && top.x < right.x && left.y > top.y && left.y < bottom.y {
                alignment_sum += top.x * left.y;
            }
        }
    }
    eprintln!("Part 1 = {:?}", alignment_sum);

    let mut input = vec![];

    let mut is_horizontal = true;
    while let Some((p1, p2)) = (if is_horizontal { &paths_x } else { &paths_y })
        .iter()
        .find(|&(p1, p2)| *p1 == pos || *p2 == pos)
    {
        let end = if pos == *p1 { *p2 } else { *p1 };
        let (new_dir, turn) = match dir {
            Dir::North if end.x < pos.x => (Dir::West, 'L'),
            Dir::North => (Dir::East, 'R'),
            Dir::East if end.y < pos.y => (Dir::North, 'L'),
            Dir::East => (Dir::South, 'R'),
            Dir::South if end.x > pos.x => (Dir::East, 'L'),
            Dir::South => (Dir::West, 'R'),
            Dir::West if end.y > pos.y => (Dir::South, 'L'),
            Dir::West => (Dir::North, 'R'),
        };
        input.push(turn.to_string());

        let distance = cmp::max(
            (p2.x as i32 - p1.x as i32).abs(),
            (p2.y as i32 - p1.y as i32).abs(),
        );
        input.push(distance.to_string());
        dir = new_dir;

        is_horizontal = !is_horizontal;
        pos = end;
    }

    eprintln!("input = {:?}", input);

    let input2: Vec<i64> =
        "A,B,B,C,B,C,B,C,A,A\nL,6,R,8,L,4,R,8,L,12\nL,12,R,10,L,4\nL,12,L,6,L,4,L,4\nn\n"
            .chars()
            .map(|c| c as u32 as i64)
            .rev()
            .collect();
    let part2 = Program::new(mem.clone(), 2).calculate(input2);
    let output2 = part2
        .iter()
        .map(|i| char::from_u32(*i as u32).unwrap())
        .collect::<String>();
    eprintln!("Part 2:\n{}", output2);
    eprintln!("Answer = {:?}", part2.last().unwrap());
}

struct Program {
    i: usize,
    instruction: i64,
    relative_base: i64,
    mem: Vec<i64>,
}

impl Program {
    fn new(mut mem: Vec<i64>, input: i64) -> Self {
        mem[0] = input;
        Self {
            i: 0,
            instruction: 0,
            relative_base: 0,
            mem,
        }
    }

    fn calculate(&mut self, mut input: Vec<i64>) -> Vec<i64> {
        let mut outputs = vec![];
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
                    self.write(1, input.pop().unwrap());
                    self.i += 2;
                }
                4 => {
                    outputs.push(self.param(1));
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
                99 => return outputs,
                n => panic!(format!("{}", n)),
            }
        }
        outputs
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
