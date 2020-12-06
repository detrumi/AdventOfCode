use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_6.txt");

fn part1() -> usize {
    let mut result = 0;
    for block in INPUT.split("\n\n") {
        let mut yes = HashSet::new();

        for line in block.lines() {
            for c in line.chars() {
                yes.insert(c);
            }
        }
        result += yes.len()
    }
    result
}

fn part2() -> usize {
    let mut result = 0;
    for block in INPUT.split("\n\n") {
        let mut yes: Option<HashSet<char>> = None;

        for line in block.lines() {
            let mut yes_line = HashSet::new();
            for c in line.chars() {
                yes_line.insert(c);
            }
            if yes.is_some() {
                yes = Some(
                    yes.unwrap()
                        .intersection(&yes_line.into_iter().collect())
                        .cloned()
                        .collect(),
                );
            } else {
                yes = Some(yes_line.into_iter().collect());
            }
        }
        result += yes.unwrap().len()
    }
    result
}
fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
