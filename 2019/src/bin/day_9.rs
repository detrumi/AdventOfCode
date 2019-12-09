use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_9.txt").unwrap();
    let codes: HashMap<usize, i64> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            l.unwrap()
                .split(",")
                .map(|s| s.parse::<i64>().unwrap())
                .enumerate()
                .collect()
        })
        .next()
        .unwrap();

    Program::new(codes.clone()).calculate(1);
    Program::new(codes).calculate(2);
}

struct Program {
    i: usize,
    relative_base: i64,
    inputs: Vec<i64>,
    codes: HashMap<usize, i64>,
}

impl Program {
    fn new(codes: HashMap<usize, i64>) -> Self {
        Self {
            i: 0,
            relative_base: 0,
            inputs: vec![],
            codes,
        }
    }

    fn calculate(&mut self, input_id: i64) {
        while let Some(code) = self.codes.get(&self.i) {
            self.inputs = self.inputs(*code);
            match code % 100 {
                1 => {
                    *self.param(3) = *self.param(1) + *self.param(2);
                    self.i += 4;
                }
                2 => {
                    *self.param(3) = *self.param(1) * *self.param(2);
                    self.i += 4;
                }
                3 => {
                    *self.param(1) = input_id;
                    self.i += 2;
                }
                4 => {
                    println!("Output = {}", *self.param(1));
                    self.i += 2;
                }
                5 => {
                    self.i = if *self.param(1) != 0 {
                        *self.param(2) as usize
                    } else {
                        self.i + 3
                    }
                }
                6 => {
                    self.i = if *self.param(1) == 0 {
                        *self.param(2) as usize
                    } else {
                        self.i + 3
                    };
                }
                7 => {
                    *self.param(3) = (*self.param(1) < *self.param(2)) as i64;
                    self.i += 4;
                }
                8 => {
                    *self.param(3) = (*self.param(1) == *self.param(2)) as i64;
                    self.i += 4;
                }
                9 => {
                    self.relative_base += *self.param(1);
                    self.i += 2;
                }
                99 => return,
                n => panic!(format!("{}", n)),
            }
        }
    }

    fn param(&mut self, index: usize) -> &mut i64 {
        let target = match self.inputs[index - 1] {
            0 => *self.codes.entry(self.i + index).or_default(),
            1 => (self.i + index) as i64,
            2 => *self.codes.entry(self.i + index as usize).or_default() + self.relative_base,
            _ => panic!(),
        };
        self.codes.entry(target as usize).or_default()
    }

    fn inputs(&self, code: i64) -> Vec<i64> {
        let mut inputs = vec![];
        let mut n = code / 100;
        while n > 0 {
            inputs.push(n % 10);
            n /= 10;
        }
        while inputs.len() < 3 {
            inputs.push(0);
        }
        inputs
    }
}
