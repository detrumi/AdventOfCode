use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_6.txt");

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
enum Op {
    Add,
    Mult,
}

fn parse() -> (Vec<Vec<usize>>, Vec<Op>) {
    let mut input = INPUT
        .trim()
        .lines()
        .map(|line| line.split_ascii_whitespace().collect_vec())
        .collect_vec();
    let ops = input
        .pop()
        .unwrap()
        .into_iter()
        .map(|s| if s == "+" { Op::Add } else { Op::Mult })
        .collect();
    let numbers = input
        .into_iter()
        .map(|line| line.iter().map(|s| s.parse().unwrap()).collect())
        .collect();
    (numbers, ops)
}

fn part1() -> usize {
    let (numbers, ops) = parse();
    (0..ops.len())
        .map(|x| {
            numbers.iter().map(|row| row[x]).fold(
                if ops[x] == Op::Add { 0 } else { 1 },
                |acc, n| {
                    if ops[x] == Op::Add { acc + n } else { acc * n }
                },
            )
        })
        .sum()
}

fn part2() -> usize {
    let mut input = INPUT
        .lines()
        .map(|line| line.chars().collect_vec())
        .collect_vec();
    let mut result = 0;
    let ops = input.pop().unwrap();
    let mut op = Op::Add;
    let mut num = 0;
    for (x, c) in ops.iter().enumerate() {
        op = match c {
            '+' => {
                num = 0;
                Op::Add
            }
            '*' => {
                num = 1;
                Op::Mult
            }
            _ => op,
        };
        let s = input.iter().map(|row| row[x]).join("").trim().to_string();
        if s.is_empty() {
            result += num;
        } else if op == Op::Add {
            num += s.parse::<usize>().unwrap();
        } else {
            num *= s.parse::<usize>().unwrap();
        }
    }
    result + num
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2()); // 9630000824360 too low
}
