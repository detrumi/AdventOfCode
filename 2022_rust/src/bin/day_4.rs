use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_4.txt");

fn parse() -> Vec<Vec<Vec<usize>>> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            line.split(',')
                .map(|s| s.split('-').map(|s| s.parse::<usize>().unwrap()).collect())
                .collect()
        })
        .collect()
}

fn part1() -> usize {
    let mut result = 0;
    for parts in parse() {
        let (left, right) = (&parts[0], &parts[1]);
        if left[0] <= right[0] && left[1] >= right[1] {
            result += 1;
        } else if right[0] <= left[0] && right[1] >= left[1] {
            result += 1;
        }
    }
    result
}

fn part2() -> usize {
    let mut result = 0;
    for parts in parse() {
        let left: HashSet<_> = (parts[0][0]..=parts[0][1]).collect();
        let right: HashSet<_> = (parts[1][0]..=parts[1][1]).collect();
        if left.intersection(&right).next().is_some() {
            result += 1;
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
