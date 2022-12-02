use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_2.txt");

fn part1() -> usize {
    let mut result = 0;
    for line in INPUT.trim().lines() {
        let parts = line.split_ascii_whitespace().collect_vec();
        result += match (parts[0], parts[1]) {
            ("A", "X") => 3 + 1,
            ("B", "Y") => 3 + 2,
            ("C", "Z") => 3 + 3,
            ("A", "Y") => 6 + 2,
            ("B", "Z") => 6 + 3,
            ("C", "X") => 6 + 1,
            ("A", "Z") => 0 + 3,
            ("B", "X") => 0 + 1,
            ("C", "Y") => 0 + 2,
            _ => panic!(),
        }
    }
    result
}

fn part2() -> usize {
    let mut result = 0;
    for line in INPUT.trim().lines() {
        let parts = line.split_ascii_whitespace().collect_vec();
        result += match (parts[0], parts[1]) {
            ("A", "X") => 0 + 3,
            ("B", "X") => 0 + 1,
            ("C", "X") => 0 + 2,
            ("A", "Y") => 3 + 1,
            ("B", "Y") => 3 + 2,
            ("C", "Y") => 3 + 3,
            ("A", "Z") => 6 + 2,
            ("B", "Z") => 6 + 3,
            ("C", "Z") => 6 + 1,
            _ => panic!(),
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
