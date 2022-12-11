use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_11.txt");

#[derive(Debug)]
struct Monkey {
    items: Vec<usize>,
    op: (String, Option<usize>),
    div: usize,
    t: usize,
    f: usize,
    inspections: usize,
}

fn parse() -> Vec<Monkey> {
    INPUT
        .trim()
        .split("\n\n")
        .map(|part| {
            let lines = part.lines().collect_vec();
            let ops = lines[1].split_ascii_whitespace().collect_vec();
            Monkey {
                items: lines[0]
                    .split_ascii_whitespace()
                    .map(|s| s.parse::<usize>().unwrap())
                    .collect_vec(),
                op: (ops[0].to_string(), ops[1].parse().ok()),
                div: lines[2].parse().unwrap(),
                t: lines[3].parse().unwrap(),
                f: lines[4].parse().unwrap(),
                inspections: 0,
            }
        })
        .collect()
}

fn solve(part2: bool) -> usize {
    let mut monkeys = parse();
    let gcd: usize = monkeys.iter().map(|m| m.div).product();

    let rounds = if part2 { 10_000 } else { 20 };
    for _round in 1..=rounds {
        for m in 0..monkeys.len() {
            while !monkeys[m].items.is_empty() {
                let r = monkeys[m].op.1.unwrap_or(monkeys[m].items[0]);
                match monkeys[m].op.0.as_str() {
                    "+" => monkeys[m].items[0] += r,
                    _ => monkeys[m].items[0] *= r,
                }
                if part2 {
                    monkeys[m].items[0] %= gcd;
                } else {
                    monkeys[m].items[0] /= 3;
                }
                monkeys[m].inspections += 1;
                let target = if monkeys[m].items[0] % monkeys[m].div == 0 {
                    monkeys[m].t
                } else {
                    monkeys[m].f
                };
                let old = monkeys[m].items[0];
                monkeys[target].items.push(old);
                monkeys[m].items.remove(0);
            }
        }
    }
    monkeys.sort_by_key(|m| -(m.inspections as isize));
    monkeys[0].inspections * monkeys[1].inspections
}

fn main() {
    println!("Part 1: {}", solve(false));
    println!("Part 2: {}", solve(true));
}
