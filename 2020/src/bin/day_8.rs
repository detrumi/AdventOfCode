use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_8.txt");

#[derive(Clone, Copy, Debug)]
enum Op {
    Acc,
    Jmp,
    Nop,
}

fn parse() -> Vec<(Op, i32)> {
    INPUT
        .lines()
        .map(|line| {
            let parts: Vec<_> = line.split_ascii_whitespace().collect();
            let instr = match parts[0] {
                "acc" => Op::Acc,
                "jmp" => Op::Jmp,
                "nop" => Op::Nop,
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
        if i as usize >= input.len() {
            return Err(acc);
        }

        let (op, num) = &input[i as usize];
        match op {
            Op::Acc => acc += num,
            Op::Jmp => i += num - 1,
            Op::Nop => (),
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
            Op::Acc => continue,
            Op::Jmp => input[i].0 = Op::Nop,
            Op::Nop => input[i].0 = Op::Jmp,
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
