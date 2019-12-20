use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_19.txt").unwrap();
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

    let mut num_affected = 0;
    for y in 0_i64..50_i64 {
        for x in 0_i64..50_i64 {
            let result = Program::new(mem.clone()).calculate(vec![x, y])[0];
            if result == 1 {
                num_affected += 1;
            }
        }
    }
    println!("Part 1 = {:?}", num_affected);

    let mut prev_bounds = vec![];
    const WIDTH: i64 = 100;
    for y in WIDTH.. {
        let mut left_most = None;
        for x in 0.. {
            let result = Program::new(mem.clone()).calculate(vec![x, y])[0];
            match left_most {
                None if result == 1 => {
                    left_most = Some(x);
                }
                Some(left_most) if result == 0 => {
                    let width = x - left_most;
                    prev_bounds.push(x - 1);
                    if width >= WIDTH {
                        let prev_r = prev_bounds[prev_bounds.len() - WIDTH as usize];
                        let y_start = y - WIDTH + 1;
                        if prev_r as i32 - left_most as i32 + 1 >= WIDTH as i32 {
                            println!(
                                "Part 2 = ({},{}) => {}",
                                left_most,
                                y_start,
                                10_000 * left_most + y_start
                            );
                            return;
                        }
                    }
                    break;
                }
                _ => (),
            }
        }
    }
}

struct Program {
    i: usize,
    instruction: i64,
    relative_base: i64,
    mem: Vec<i64>,
}

impl Program {
    fn new(mem: Vec<i64>) -> Self {
        Self {
            i: 0,
            instruction: 0,
            relative_base: 0,
            mem,
        }
    }

    fn calculate(&mut self, mut inputs: Vec<i64>) -> Vec<i64> {
        inputs = inputs.iter().rev().copied().collect();
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
                    self.write(1, inputs.pop().unwrap());
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
