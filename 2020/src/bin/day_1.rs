use itertools::iproduct;

const INPUT: &str = include_str!("../../input/day_1.txt");

fn parse() -> Vec<usize> {
    INPUT.lines().map(|l| l.parse::<usize>().unwrap()).collect()
}

fn part1() -> usize {
    let input = parse();
    let (a, b) = iproduct!(&input, &input)
        .find(|(&a, &b)| a + b == 2020)
        .unwrap();
    a * b
}

fn part2() -> usize {
    let input = parse();
    let (a, b, c) = iproduct!(&input, &input, &input)
        .find(|(&a, &b, &c)| a + b + c == 2020)
        .unwrap();
    a * b * c
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
