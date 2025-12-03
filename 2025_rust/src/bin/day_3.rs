use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_3.txt");

fn parse() -> impl Iterator<Item = Vec<usize>> {
    INPUT.trim().lines().map(|line| {
        line.chars()
            .map(|c| c.to_digit(10).unwrap() as usize)
            .collect()
    })
}

fn first_max(slice: &[usize]) -> usize {
    // position_max gets last maximum so we reverse the slice and index
    slice.len() - 1 - slice.iter().rev().position_max().unwrap()
}

fn part1() -> usize {
    parse()
        .map(|line| {
            let pos = first_max(&line[..line.len() - 1]);
            10 * line[pos] + line[pos + 1..].iter().max().unwrap()
        })
        .sum()
}

fn part2() -> usize {
    parse()
        .map(|line| {
            let mut start = 0;
            (0..12).rev().fold(0, |result, n| {
                let pos = start + first_max(&line[start..line.len() - n]);
                start = pos + 1;
                10 * result + line[pos]
            })
        })
        .sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
