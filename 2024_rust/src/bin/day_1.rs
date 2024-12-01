use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_1.txt");

fn parse() -> (Vec<usize>, Vec<usize>) {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            line.split_ascii_whitespace()
                .map(|n| n.parse::<usize>().unwrap())
                .collect_tuple()
                .unwrap()
        })
        .collect()
}

fn part1() -> usize {
    let (l, r) = parse();
    l.iter()
        .sorted()
        .zip(r.iter().sorted())
        .map(|(&a, &b)| a.abs_diff(b))
        .sum()
}

fn part2() -> usize {
    let (l, r) = parse();
    l.iter()
        .map(|a| a * r.iter().filter(|n| *n == a).count())
        .sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
