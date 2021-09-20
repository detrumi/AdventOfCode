use crossbeam_channel::unbounded;
use std::{
    ops::{AddAssign, MulAssign, RemAssign},
    sync::{Arc, Mutex},
    thread,
};

const INPUT: &str = include_str!("../../input/day_18.txt");

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

impl AddAssign for Frequency {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl MulAssign for Frequency {
    fn mul_assign(&mut self, rhs: Self) {
        self.0 *= rhs.0;
    }
}

impl RemAssign for Frequency {
    fn rem_assign(&mut self, rhs: Self) {
        self.0 %= rhs.0;
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
    Send(Value),
    Set(Register, Value),
    Add(Register, Value),
    Mul(Register, Value),
    Mod(Register, Value),
    Recover(Register),
    JumpGreaterThanZero(Value, Value),
}

fn parse() -> Vec<Instruction> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let parts: Vec<_> = line.split_ascii_whitespace().collect();
            match parts[0] {
                "snd" => Instruction::Send(parts[1].into()),
                "set" => Instruction::Set(parts[1].into(), parts[2].into()),
                "add" => Instruction::Add(parts[1].into(), parts[2].into()),
                "mul" => Instruction::Mul(parts[1].into(), parts[2].into()),
                "mod" => Instruction::Mod(parts[1].into(), parts[2].into()),
                "rcv" => Instruction::Recover(parts[1].into()),
                "jgz" => Instruction::JumpGreaterThanZero(parts[1].into(), parts[2].into()),
                x => panic!("Unrecognized instruction: `{}`", x),
            }
        })
        .collect()
}

fn read(value: Value, registers: &Vec<Frequency>) -> Frequency {
    match value {
        Value::Register(r) => registers[r.0],
        Value::Frequency(f) => f,
    }
}

fn part1() -> i64 {
    let mut registers = vec![Frequency(0); 26];
    let mut sound = Frequency(0);
    let instructions = parse();
    let mut i = 0;
    while i >= 0 && i < instructions.len() as i32 {
        match instructions[i as usize] {
            Instruction::Send(v) => {
                sound = read(v, &registers);
            }
            Instruction::Set(r, v) => registers[r.0] = read(v, &registers),
            Instruction::Add(r, v) => {
                let f = read(v, &registers);
                registers[r.0] += f;
            }
            Instruction::Mul(r, v) => {
                let f = read(v, &registers);
                registers[r.0] *= f;
            }
            Instruction::Mod(r, v) => {
                let f = read(v, &registers);
                registers[r.0] %= f;
            }
            Instruction::Recover(r) => {
                if registers[r.0].0 > 0 {
                    registers[r.0] = sound;
                    return sound.0;
                }
            }
            Instruction::JumpGreaterThanZero(v1, v2) => {
                let f1 = read(v1, &registers);
                let f2 = read(v2, &registers);
                if f1.0 > 0 {
                    i += f2.0 as i32 - 1;
                }
            }
        }
        i += 1;
    }
    unreachable!()
}

fn part2() -> usize {
    let parsed = parse();
    let deadlock = Arc::new(Mutex::new(false));
    let channels = vec![unbounded(), unbounded()];
    let mut threads = vec![];
    for id in 0..2 {
        let instructions = parsed.clone();

        let deadlock = Arc::clone(&deadlock);
        let sender = channels[id].0.clone();
        let receiver = channels[1 - id].1.clone();

        threads.push(thread::spawn(move || {
            let mut counter = 0;

            let mut registers = vec![Frequency(0); 26];
            registers['p' as usize - 'a' as usize] = Frequency(id as i64);
            let mut i = 0;
            while i >= 0 && i < instructions.len() as i32 {
                match instructions[i as usize] {
                    Instruction::Send(v) => {
                        let f = read(v, &registers);
                        *deadlock.lock().unwrap() = false;
                        sender.send(Some(f)).unwrap();
                        counter += 1;
                    }
                    Instruction::Set(r, v) => registers[r.0] = read(v, &registers),
                    Instruction::Add(r, v) => {
                        let f = read(v, &registers);
                        registers[r.0] += f;
                    }
                    Instruction::Mul(r, v) => {
                        let f = read(v, &registers);
                        registers[r.0] *= f;
                    }
                    Instruction::Mod(r, v) => {
                        let f = read(v, &registers);
                        registers[r.0] %= f;
                    }
                    Instruction::Recover(r) => {
                        if receiver.is_empty() {
                            let mut deadlocked = deadlock.lock().unwrap();
                            if *deadlocked {
                                sender.send(None).unwrap();
                                return counter;
                            }
                            *deadlocked = true;
                        }
                        if let Some(received) = receiver.recv().unwrap() {
                            registers[r.0] = received;
                        } else {
                            return counter;
                        }
                    }
                    Instruction::JumpGreaterThanZero(v1, v2) => {
                        let f1 = read(v1, &registers);
                        let f2 = read(v2, &registers);
                        if f1.0 > 0 {
                            i += f2.0 as i32 - 1;
                        }
                    }
                }
                i += 1;
            }
            unreachable!()
        }));
    }

    let mut result = 0;
    for thread in threads {
        result = thread.join().unwrap();
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
