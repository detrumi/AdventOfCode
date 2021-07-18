use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead};

#[allow(non_snake_case)]
enum Op {
    AddRegister,
    AddImmediate,
    MulRegister,
    MulImmediate,
    AndRegister,
    AndImmediate,
    OrRegister,
    OrImmediate,
    SetRegister,
    SetImmediate,
    GreaterThanIR,
    GreaterThanRI,
    GreaterThanRR,
    EqIR,
    EqRI,
    EqRR,
}

use crate::Op::*;

type Instruction = Vec<usize>;
type Registers = Vec<usize>;

impl Op {
    pub fn all_ops() -> Vec<Op> {
        vec![
            AddRegister,
            AddImmediate,
            MulRegister,
            MulImmediate,
            AndRegister,
            AndImmediate,
            OrRegister,
            OrImmediate,
            SetRegister,
            SetImmediate,
            GreaterThanIR,
            GreaterThanRI,
            GreaterThanRR,
            EqIR,
            EqRI,
            EqRR,
        ]
    }

    fn op_result(&self, reg: &Registers, a: usize, b: usize) -> usize {
        match self {
            AddRegister => reg[a] + reg[b],
            AddImmediate => reg[a] + b,
            MulRegister => reg[a] * reg[b],
            MulImmediate => reg[a] * b,
            AndRegister => reg[a] & reg[b],
            AndImmediate => reg[a] & b,
            OrRegister => reg[a] | reg[b],
            OrImmediate => reg[a] | b,
            SetRegister => reg[a],
            SetImmediate => a,
            other => {
                if match other {
                    GreaterThanIR => a > reg[b],
                    GreaterThanRI => reg[a] > b,
                    GreaterThanRR => reg[a] > reg[b],
                    EqIR => a == reg[b],
                    EqRI => reg[a] == b,
                    EqRR => reg[a] == reg[b],
                    _ => unreachable!(),
                } {
                    1
                } else {
                    0
                }
            }
        }
    }

    fn execute(&self, mut reg: Registers, instruction: &Instruction) -> Registers {
        let a = instruction[1];
        let b = instruction[2];
        let c = instruction[3];
        reg[c] = self.op_result(&reg, a, b);
        reg
    }
}

fn main() {
    let file = File::open("input/day_16.txt").unwrap();
    let mut lines = io::BufReader::new(file).lines().map(|l| l.unwrap());

    let mut part_1 = 0;
    let mut possible_ops: Vec<HashSet<usize>> = vec![(0..16).collect(); 16];
    while let Some(before) = read_sample_state(&lines.next().unwrap()) {
        let instruction = read_instruction(&lines.next().unwrap());
        let after = read_sample_state(&lines.next().unwrap()).unwrap();
        let _empty_line = lines.next();

        let mut num_opcodes = 0;
        let mut ops: HashSet<usize> = HashSet::new();
        for (i, op) in Op::all_ops().iter().enumerate() {
            if op.execute(before.clone(), &instruction) == after {
                num_opcodes += 1;
                ops.insert(i);
            }
        }
        possible_ops[instruction[0]] = possible_ops[instruction[0]]
            .intersection(&ops)
            .cloned()
            .collect();

        if num_opcodes >= 3 {
            part_1 += 1;
        }
    }

    println!("Part 1: {}", part_1);

    let mut op_numbers: HashMap<usize, usize> = HashMap::new();
    while op_numbers.len() < 16 {
        let (i, op) = possible_ops
            .iter()
            .enumerate()
            .filter(|(_, possibilities)| possibilities.len() == 1)
            .map(|(i, ps)| (i, *ps.iter().next().unwrap()))
            .next()
            .unwrap();
        op_numbers.insert(i, op);
        possible_ops[i].clear();
        for p in &mut possible_ops {
            p.remove(&op);
        }
    }

    let mut registers = vec![0; 4];
    let all_ops = Op::all_ops();
    for line in lines.filter(|l| !l.is_empty()) {
        let instruction = read_instruction(&line);
        let op = &all_ops[op_numbers[&instruction[0]]];
        registers = op.execute(registers, &instruction);
    }
    println!("Part 2: {}", registers[0]);
}

fn read_sample_state(s: &str) -> Option<Registers> {
    if s.is_empty() {
        None
    } else {
        Some(
            s.split('[')
                .nth(1)?
                .split(']')
                .nth(0)?
                .split(", ")
                .map(|s| s.parse::<usize>().unwrap())
                .collect(),
        )
    }
}

fn read_instruction(s: &str) -> Instruction {
    s.split(' ').map(|s| s.parse::<usize>().unwrap()).collect()
}
