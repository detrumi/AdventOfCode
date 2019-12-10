use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_9.txt").unwrap();
    let mem: Vec<i64> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            l.unwrap()
                .split(",")
                .map(|s| s.parse::<i64>().unwrap())
                .collect()
        })
        .next()
        .unwrap();

    Program::new(mem.clone()).calculate(1);
    Program::new(mem).calculate(2);
}

struct Program {
    i: usize,
    relative_base: i64,
    modes: Vec<i64>,
    mem: Vec<i64>,
}

impl Program {
    fn new(mem: Vec<i64>) -> Self {
        Self {
            i: 0,
            relative_base: 0,
            modes: vec![],
            mem,
        }
    }

    fn calculate(&mut self, input_id: i64) {
        while let Some(&instruction) = self.mem.get(self.i) {
            let code = instruction % 100;
            self.modes = vec![
                (instruction / 100) % 10,
                (instruction / 1_000) % 10,
                (instruction / 10_000) % 10,
            ];
            match code {
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
                    self.write(1, input_id);
                    self.i += 2;
                }
                4 => {
                    println!("Output = {}", self.param(1));
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
        match self.modes[index - 1] {
            0 => self.mem[self.i + index] as usize,
            1 => (self.i + index),
            2 => self.relative_base as usize + self.mem[self.i + index as usize] as usize,
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
