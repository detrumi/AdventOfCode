use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_7.txt");

fn parse() -> Vec<isize> {
    INPUT
        .trim()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1() -> isize {
    let crabs = parse();
    crabs
        .iter()
        .map(|crab| crabs.iter().map(|c| (crab - c).abs()).sum())
        .min()
        .unwrap()
}

fn summation(n: isize) -> isize {
    (n * (n + 1)) / 2
}

fn part2() -> isize {
    let crabs = parse();
    let (min, max) = crabs.iter().minmax().into_option().unwrap();
    (*min..=*max)
        .map(|mid| {
            crabs
                .iter()
                .map(|crab| {
                    let left = mid.min(*crab);
                    let right = mid.max(*crab);
                    summation(right - left)
                })
                .sum()
        })
        .min()
        .unwrap()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
