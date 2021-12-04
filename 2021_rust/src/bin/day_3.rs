const INPUT: &str = include_str!("../../input/day_3.txt");

fn parse() -> Vec<Vec<char>> {
    INPUT
        .trim()
        .lines()
        .map(|line| line.chars().collect())
        .collect()
}

fn to_decimal(s: String) -> usize {
    usize::from_str_radix(&s, 2).unwrap()
}

fn bool_to_bit(b: bool) -> char {
    char::from_digit(b as u32, 10).unwrap()
}

fn count(lines: &[Vec<char>], index: usize) -> usize {
    lines.iter().filter(|v| v[index] == '1').count()
}

fn part1() -> usize {
    let lines = parse();
    let gamma: String = (0..lines[0].len())
        .map(|i| bool_to_bit(2 * count(&lines, i) >= lines.len()))
        .collect();
    let gamma = to_decimal(gamma);
    let epsilon = gamma ^ ((1 << lines[0].len()) - 1);
    gamma * epsilon
}

fn part2() -> usize {
    let lines = parse();
    let mut oxygen = lines.clone();
    let mut scrubber = lines.clone();

    for n in 0..lines[0].len() {
        if oxygen.len() > 1 {
            let bit = bool_to_bit(2 * count(&oxygen, n) >= oxygen.len());
            oxygen.retain(|v| v[n] == bit);
        }
        if scrubber.len() > 1 {
            let bit = bool_to_bit(2 * count(&scrubber, n) < scrubber.len());
            scrubber.retain(|v| v[n] == bit);
        }
    }
    to_decimal(oxygen[0].iter().collect()) * to_decimal(scrubber[0].iter().collect())
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
