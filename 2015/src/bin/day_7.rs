use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_7.txt");

#[derive(Clone, Debug)]
enum Op<'a> {
    Const(u16),
    Id(&'a str),
    ConstAnd(u16, &'a str),
    And(&'a str, &'a str),
    Or(&'a str, &'a str),
    Not(&'a str),
    Lshift(&'a str, u16),
    Rshift(&'a str, u16),
}

impl<'a> Op<'a> {
    pub fn parse(words: &[&'a str]) -> Self {
        match words.len() {
            1 => {
                if let Ok(n) = words[0].parse() {
                    Op::Const(n)
                } else {
                    Op::Id(words[0])
                }
            }
            2 => Op::Not(words[1]),
            3 => {
                let l = words[0];
                let r = words[2];
                match words[1] {
                    "AND" => {
                        if let Ok(n) = words[0].parse() {
                            Op::ConstAnd(n, r)
                        } else {
                            Op::And(l, r)
                        }
                    }
                    "OR" => Op::Or(l, r),
                    "LSHIFT" => Op::Lshift(l, r.parse().unwrap()),
                    "RSHIFT" => Op::Rshift(l, r.parse().unwrap()),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }
}

fn eval<'a>(wire: &'a str, values: &mut HashMap<&'a str, Op<'a>>) -> u16 {
    let op = values.get(&wire).unwrap().clone();
    let result = match op {
        Op::Const(n) => n,
        Op::Id(a) => eval(a, values),
        Op::ConstAnd(n, r) => n & eval(r, values),
        Op::And(l, r) => eval(l, values) & eval(r, values),
        Op::Or(l, r) => eval(l, values) | eval(r, values),
        Op::Not(a) => eval(a, values) ^ std::u16::MAX,
        Op::Lshift(a, n) => eval(a, values) << n,
        Op::Rshift(a, n) => eval(a, values) >> n,
    };
    values.insert(wire, Op::Const(result));
    result
}

fn get_values<'a>() -> HashMap<&'a str, Op<'a>> {
    let mut values: HashMap<&str, Op> = HashMap::new();
    for line in INPUT.lines() {
        let words: Vec<_> = line.split_ascii_whitespace().collect();

        let op = Op::parse(&words[0..words.len() - 2]);
        let target = words.last().unwrap();
        values.insert(target, op);
    }
    values
}

fn part1() -> u16 {
    eval("a", &mut get_values())
}

fn part2() -> u16 {
    let mut values = get_values();
    let a = eval("a", &mut values.clone());
    values.insert("b", Op::Const(a));
    eval("a", &mut values)
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
