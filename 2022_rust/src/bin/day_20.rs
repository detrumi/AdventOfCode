use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_20.txt");

fn parse() -> Vec<isize> {
    INPUT
        .trim()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part1() -> isize {
    let mut numbers = parse().into_iter().enumerate().collect_vec();
    let len = numbers.len();
    let normalize = |n| (n + 3 * len as isize) as usize % len;
    for i in 0..len {
        let delta = numbers.iter().find(|t| t.0 == i).unwrap().1;
        let index = numbers.iter().position(|t| t.0 == i).unwrap();
        let dir = delta.signum();
        for j in 0..delta.abs() {
            numbers.swap(
                normalize(index as isize + dir * j),
                normalize(index as isize + dir * (j + 1)),
            );
        }
    }

    let zero_pos = numbers.iter().position(|t| t.1 == 0).unwrap();
    (1..=3)
        .map(|i| {
            let pos = (zero_pos + 1000 * i) % len;
            numbers[pos].1
        })
        .sum()
}

fn part2() -> isize {
    let mut numbers = parse()
        .into_iter()
        .map(|n| n * 811_589_153)
        .enumerate()
        .collect_vec();
    let len = numbers.len();
    let cycle_length = len * (len - 1);
    let normalize = |n| (n + 3 * len as isize) as usize % len;
    for round in 0..10 {
        for i in 0..len {
            let index = numbers.iter().position(|t| t.0 == i).unwrap();
            let delta = numbers[index].1 % cycle_length as isize;
            println!("{round}/{i}: delta = {:?}", delta);
            let dir = delta.signum();
            for j in 0..delta.abs() {
                numbers.swap(
                    normalize(index as isize + dir * j % len as isize),
                    normalize(index as isize + dir * (j + 1) % len as isize),
                );
            }
        }
    }

    let zero_pos = numbers.iter().position(|t| t.1 == 0).unwrap();
    (1..=3)
        .map(|i| {
            let pos = (zero_pos + 1000 * i) % len;
            numbers[pos].1
        })
        .sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
