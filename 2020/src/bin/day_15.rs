use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_15.txt");

fn solve(nth: usize) -> usize {
    let input: Vec<_> = INPUT
        .split(',')
        .map(|line| line.parse::<usize>().unwrap())
        .collect();
    let mut said: HashMap<usize, (usize, usize)> = HashMap::new();
    let mut old_number = 0;
    for turn in 1..=nth {
        let number = if turn <= input.len() {
            input[turn - 1]
        } else {
            old_number
        };
        if let Some(&(turn_a, turn_b)) = said.get(&number) {
            let result = if turn_a == 0 { 0 } else { turn_b - turn_a };
            if let Some(&(_turn_c, turn_d)) = said.get(&result) {
                said.insert(result, (turn_d, turn));
            } else {
                said.insert(result, (0, turn));
            }
            old_number = result;
        } else {
            said.insert(number, (0, turn));
            old_number = number;
        }
    }
    old_number
}

fn main() {
    println!("Part 1: {}", solve(2020));
    println!("Part 2: {}", solve(30000000));
}
