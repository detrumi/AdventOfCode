use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_8.txt");

#[derive(Clone, Copy, Debug)]
enum Op {
    Acc,
    Jmp,
    Nop,
}

use Op::*;

fn parse() -> Vec<(Op, i32)> {
    INPUT
        .lines()
        .map(|line| {
            let parts: Vec<_> = line.split_ascii_whitespace().collect();
            let instr = match parts[0] {
                "acc" => Acc,
                "jmp" => Jmp,
                "nop" => Nop,
                _ => panic!(format!("Unknown op: {}", parts[0])),
            };
            let num = parts[1].parse::<i32>().unwrap();
            (instr, num)
        })
        .collect()
}

fn solve(input: Vec<(Op, i32)>) -> Result<i32, i32> {
    let mut i = 0_i32;
    let mut acc = 0;
    let mut executed = HashSet::new();
    while executed.insert(i) {
        let (op, num) = input.get(i as usize).ok_or(acc)?;
        match op {
            Acc => acc += num,
            Jmp => i += num - 1,
            Nop => (),
        }
        i += 1;
    }
    Ok(acc)
}

fn part1() -> i32 {
    solve(parse()).unwrap()
}

fn part2() -> i32 {
    let initial_input = parse();
    for (i, (op, _num)) in initial_input.iter().enumerate() {
        let mut input = initial_input.clone();
        match *op {
            Acc => continue,
            Jmp => input[i].0 = Nop,
            Nop => input[i].0 = Jmp,
        }
        if let Err(acc) = solve(input) {
            return acc;
        }
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
