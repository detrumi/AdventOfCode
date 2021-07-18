use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    println!("Part 1: {}", calculate(20));

    let one_thousand: i64 = calculate(1_000);
    let two_thousand: i64 = calculate(2_000);
    let thousands_needed: i64 = 50_000_000 - 1;
    println!(
        "Part 2: {}",
        thousands_needed * (two_thousand - one_thousand) + one_thousand
    );
}

fn calculate(iterations: i64) -> i64 {
    let file = File::open("input/day_12.txt").unwrap();
    let mut lines = io::BufReader::new(file).lines().map(|l| l.unwrap());

    let mut state: VecDeque<bool> = lines
        .next()
        .unwrap()
        .split(' ')
        .nth(2)
        .unwrap()
        .chars()
        .map(|c| c == '#')
        .collect();

    let mut rules: Vec<(Vec<bool>, bool)> = vec![];
    for line in lines.skip(1) {
        let mut parts = line.split(" => ");
        rules.push((
            parts.next().unwrap().chars().map(|c| c == '#').collect(),
            parts.next().unwrap() == "#",
        ));
    }

    let mut left_most: i64 = 0;
    for _generation in 0..iterations {
        if (0..4).any(|i| state[i]) {
            for _ in 0..4 {
                state.push_front(false);
            }
            left_most -= 4;
        }

        if (0..4).any(|i| state[state.len() - 1 - i]) {
            for _ in 0..4 {
                state.push_back(false);
            }
        }

        let mut new_state: VecDeque<bool> = state.clone();
        for i in 0..state.len() - 4 {
            new_state[i + 2] = false;
            for (matches, value) in &rules {
                if (0..5).all(|j| state[i + j] == matches[j]) {
                    new_state[i + 2] = *value;
                    break;
                }
            }
        }
        state = new_state;
    }
    state
        .iter()
        .enumerate()
        .filter(|t| *t.1)
        .map(|t| t.0 as i64 + left_most as i64)
        .sum()
}
