use itertools::Itertools;

const INPUT: &str = "1113122113";

fn look_and_say(digits: Vec<u32>) -> Vec<u32> {
    let mut result = vec![];
    for (key, group) in &digits.iter().group_by(|c| *c) {
        result.push(group.count() as u32);
        result.push(*key);
    }
    result
}

fn part1() -> usize {
    let mut digits = INPUT.chars().filter_map(|c| c.to_digit(10)).collect();
    for _ in 0..40 {
        digits = look_and_say(digits);
    }
    digits.len()
}

fn part2() -> usize {
    let mut digits = INPUT.chars().filter_map(|c| c.to_digit(10)).collect();
    for _ in 0..50 {
        digits = look_and_say(digits);
    }
    digits.len()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
