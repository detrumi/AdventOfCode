use std::ops::{MulAssign, SubAssign};

const INPUT: &str = include_str!("../../input/day_23.txt");

#[derive(Clone, Copy, Debug)]
struct Frequency(i64);

#[derive(Clone, Copy, Debug)]
struct Register(usize);

#[derive(Clone, Copy, Debug)]
enum Value {
    Register(Register),
    Frequency(Frequency),
}

impl From<&str> for Frequency {
    fn from(s: &str) -> Self {
        Frequency(s.parse::<i64>().unwrap())
    }
}

impl SubAssign for Frequency {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0;
    }
}

impl MulAssign for Frequency {
    fn mul_assign(&mut self, rhs: Self) {
        self.0 *= rhs.0;
    }
}

impl From<&str> for Register {
    fn from(s: &str) -> Self {
        Register(s.chars().next().unwrap() as usize - 'a' as usize)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        match s.chars().next().unwrap() {
            'a'..='z' => Value::Register(s.into()),
            _ => Value::Frequency(s.into()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Instruction {
    Set(Register, Value),
    Sub(Register, Value),
    Mul(Register, Value),
    JumpNotZero(Value, Value),
}

fn parse(s: &str) -> Vec<Instruction> {
    s.trim()
        .lines()
        .map(|line| {
            let parts: Vec<_> = line.split_ascii_whitespace().collect();
            match parts[0] {
                "set" => Instruction::Set(parts[1].into(), parts[2].into()),
                "sub" => Instruction::Sub(parts[1].into(), parts[2].into()),
                "mul" => Instruction::Mul(parts[1].into(), parts[2].into()),
                "jnz" => Instruction::JumpNotZero(parts[1].into(), parts[2].into()),
                x => panic!("Unrecognized instruction: `{}`", x),
            }
        })
        .collect()
}

fn read(value: Value, registers: &[Frequency]) -> Frequency {
    match value {
        Value::Register(r) => registers[r.0],
        Value::Frequency(f) => f,
    }
}

fn part1() -> usize {
    let mut registers = [Frequency(0); 8];
    let instructions = parse(INPUT);
    let mut i = 0;
    let mut muls = 0;
    while i >= 0 && i < instructions.len() as i32 {
        match instructions[i as usize] {
            Instruction::Set(r, v) => registers[r.0] = read(v, &registers),
            Instruction::Sub(r, v) => {
                let f = read(v, &registers);
                registers[r.0] -= f;
            }
            Instruction::Mul(r, v) => {
                let f = read(v, &registers);
                registers[r.0] *= f;
                muls += 1;
            }
            Instruction::JumpNotZero(v1, v2) => {
                let f1 = read(v1, &registers);
                let f2 = read(v2, &registers);
                if f1.0 != 0 {
                    i += f2.0 as i32 - 1;
                }
            }
        }
        i += 1;
    }
    muls
}

#[allow(unused_assignments)]
fn part2() -> i64 {
    let (a, mut g, mut h) = (1, 0, 0);

    let mut b = 57;
    let mut c = b;
    if a != 0 {
        b *= 100;
        b += 100000;
        c = b;
        c += 17000;
    }
    loop {
        let mut f = true;
        let mut d = 2;
        loop {
            if b % d == 0 {
                f = false;
            }
            d += 1;
            g = d;
            g -= b;
            if g == 0 {
                break;
            }
        }
        if !f {
            h += 1;
        }
        g = b;
        g -= c;
        if g == 0 {
            return h;
        }
        b += 17;
    }
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
