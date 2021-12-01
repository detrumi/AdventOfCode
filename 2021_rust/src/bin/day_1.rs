use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_1.txt");

fn parse() -> impl Iterator<Item = usize> {
    INPUT.lines().map(|line| line.parse().unwrap())
}

fn part1() -> usize {
    let mut result = 0;
    let mut last = usize::MAX;

    for n in parse() {
        if n > last {
            result += 1;
        }
        last = n;
    }
    result
}

fn part2() -> usize {
    let mut result = 0;
    let mut last_sum = usize::MAX;
    for (a, b, c) in parse().tuple_windows() {
        let sum = a + b + c;
        if sum > last_sum {
            result += 1
        }
        last_sum = sum;
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
