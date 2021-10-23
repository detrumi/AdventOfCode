use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_19.txt");
const NUM_REGISTERS: usize = 6;

#[derive(Copy, Clone)]
struct Instruction<'a> {
    pub op: &'a str,
    pub a: usize,
    pub b: usize,
    pub c: usize,
}

fn parse<'a>() -> Option<(usize, Vec<Instruction<'a>>)> {
    let mut lines = INPUT.lines();
    let ip_register = lines
        .next()?
        .split_ascii_whitespace()
        .last()?
        .parse()
        .ok()?;

    let instructions = lines
        .filter_map(|line| line.split_ascii_whitespace().collect_tuple())
        .filter_map(|(op, a, b, c)| {
            Some(Instruction {
                op,
                a: a.parse().ok()?,
                b: b.parse().ok()?,
                c: c.parse().ok()?,
            })
        })
        .collect();

    Some((ip_register, instructions))
}

fn solve(start_value: usize) -> usize {
    let (ip_register, instructions) = parse().unwrap();
    let mut registers = [0_usize; NUM_REGISTERS];
    registers[0] = start_value;
    while registers[ip_register] < instructions.len() {
        let Instruction { op, a, b, c } = instructions[registers[ip_register]];
        registers[c as usize] = match op {
            "addr" => registers[a] + registers[b],
            "addi" => registers[a] + b,
            "mulr" => registers[a] * registers[b],
            "muli" => registers[a] * b,
            "banr" => registers[a] & registers[b],
            "bani" => registers[a] & b,
            "borr" => registers[a] | registers[b],
            "bori" => registers[a] | b,
            "setr" => registers[a],
            "seti" => a,
            other => {
                (match other {
                    "gtir" => a > registers[b],
                    "gtri" => registers[a] > b,
                    "gtrr" => registers[a] > registers[b],
                    "eqir" => a == registers[b],
                    "eqri" => registers[a] == b,
                    "eqrr" => registers[a] == registers[b],
                    _ => panic!(),
                }) as usize
            }
        };
        registers[ip_register] += 1;
    }
    registers[0]
}

fn solve_fast(start_value: usize) -> usize {
    let mut r = [0_usize; NUM_REGISTERS];
    r[0] = start_value;

    r[2] += 2;
    r[2] *= r[2];
    r[2] *= 19;
    r[2] *= 11;
    r[1] += 5;
    r[1] *= 22;
    r[1] += 8;
    r[2] += r[1];

    if r[0] == 1 {
        r[1] = 27;
        r[1] *= 28;
        r[1] += 29;
        r[1] *= 30;
        r[1] *= 14;
        r[1] *= 32;
        r[2] += r[1];
        r[0] = 0;
    }

    // r[4] = 1;
    // 'outer: loop {
    //     r[5] = 1;
    //     'inner: loop {
    //         if r[4] * r[5] == r[2] {
    //             r[0] += r[4];
    //         }
    //         r[5] += 1;
    //         if r[5] <= r[2] {
    //             continue 'inner;
    //         }
    //         r[4] += 1;
    //         if r[4] <= r[2] {
    //             continue 'outer;
    //         }
    //         eprintln!("r = {:?}", r);
    //         return r[0];
    //     }
    // }

    return (1..=r[2]).filter(|n| r[2] % n == 0).sum();
}

fn main() {
    println!("Part 1: {}", solve(0));
    println!("Part 2: {}", solve_fast(1));
}
