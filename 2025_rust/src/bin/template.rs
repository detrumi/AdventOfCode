const INPUT: &str = include_str!("../../input/day_1.txt");

fn parse() -> Vec<usize> {
    INPUT
        .trim()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part1() -> usize {
    let mut result = 0;
    for line in parse() {
        //
    }
    result
}

fn part2() -> usize {
    parse();
    0
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
