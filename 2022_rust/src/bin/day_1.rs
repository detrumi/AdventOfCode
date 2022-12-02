use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_1.txt");

fn parse() -> impl Iterator<Item = Vec<usize>> {
    INPUT.split("\n\n").map(|group| {
        group
            .lines()
            .map(|line| line.parse::<usize>().unwrap())
            .collect()
    })
}

fn part1() -> usize {
    parse()
        .map(|group| group.iter().sum::<usize>())
        .max()
        .unwrap()
}

fn part2() -> usize {
    parse()
        .map(|group| group.iter().sum::<usize>())
        .sorted()
        .rev()
        .take(3)
        .sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
