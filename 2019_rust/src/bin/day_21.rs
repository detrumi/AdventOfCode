use std::char;
use std::fs::File;
use std::io::{self, BufRead};

const PROGRAM_1: &str = "\
NOT A J
NOT B T
AND D T
OR T J
NOT C T
AND D T
OR T J
";

const PROGRAM_2: &str = "\
NOT B J
NOT C T
OR T J
NOT A T
AND A T
OR I T
AND E T
OR H T
AND T J
NOT A T
OR T J
AND D J
";

fn main() {
    let file = File::open("input/day_21.txt").unwrap();
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

    println!("Part 1:");
    run_program_with(mem.clone(), "WALK", PROGRAM_1);
    println!("Part 2:");
    run_program_with(mem, "RUN", PROGRAM_2);
}

fn run_program_with(mem: Vec<i64>, command: &str, program: &str) {
    let program = Program::new(mem).calculate(program.trim(), command);
    let output = program
        .iter()
        .filter_map(|i| char::from_u32(*i as u32))
        .collect::<String>();
    print!("{}", output);
    println!("Answer = {:?}", program.last().unwrap());
}

#[derive(Default)]
struct Program {
    i: usize,
    instruction: i64,
    relative_base: i64,
    mem: Vec<i64>,
}

impl Program {
    fn new(mem: Vec<i64>) -> Self {
        let mut program = Program::default();
        program.mem = mem;
        program
    }

    fn calculate(&mut self, input_str: &str, command: &str) -> Vec<i64> {
        let mut input: Vec<i64> = input_str
            .chars()
            .chain(format!("\n{}\n", command).chars())
            .map(|c| c as u32 as i64)
            .rev()
            .collect();
        println!("input = {:?}", input);

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
                    let value = input.pop().unwrap();
                    self.write(1, value);
                    self.i += 2;
                }
                4 => {
                    let value = self.param(1);
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
        panic!()
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
