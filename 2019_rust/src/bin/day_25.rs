use std::char;
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_25.txt").unwrap();
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

    let result = Program::new(mem.clone())
        .calculate()
        .iter()
        .map(|c| char::from_u32(*c as u32).unwrap())
        .collect::<String>();

    eprintln!("Part 1:\n{}", result);
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

    fn calculate(&mut self) -> Vec<i64> {
        let mut input: Vec<i64> = vec![];

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
                    if input.is_empty() {
                        let mut s = String::new();
                        io::stdin().lock().read_line(&mut s).unwrap();
                        s = match s.trim() {
                            "n" => "north\n".to_string(),
                            "e" => "east\n".to_string(),
                            "s" => "south\n".to_string(),
                            "w" => "west\n".to_string(),
                            _ => s,
                        };
                        input = s.chars().map(|c| c as u32 as i64).rev().collect();

                        eprintln!("input = {:?}", input);
                    }
                    self.write(1, input.pop().unwrap());
                    self.i += 2;
                }
                4 => {
                    let value = self.param(1);
                    print!("{}", char::from_u32(value as u32).unwrap());
                    outputs.push(value);
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
