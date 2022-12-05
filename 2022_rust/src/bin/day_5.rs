use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_5.txt");

fn solve(move_multiple: bool) -> String {
    let mut stacks = vec![vec![]; 10];
    let (crates, moves) = INPUT.trim_end().split_once("\n\n").unwrap();
    for line in crates.lines().rev() {
        for (i, letter) in line.chars().enumerate().filter(|(_, c)| c.is_uppercase()) {
            stacks[i / 4].push(letter);
        }
    }
    for move_ in moves.lines() {
        let parts = move_.split_ascii_whitespace().collect_vec();
        let count = parts[1].parse::<usize>().unwrap();
        let from = parts[3].parse::<usize>().unwrap() - 1;
        let to = parts[5].parse::<usize>().unwrap() - 1;

        if move_multiple {
            for i in (0..count).rev() {
                let len = stacks[from].len();
                let c = stacks[from].remove(len - i - 1);
                stacks[to].push(c);
            }
        } else {
            for _ in 0..count {
                let c = stacks[from].pop().unwrap();
                stacks[to].push(c);
            }
        }
    }
    stacks
        .iter()
        .filter(|s| !s.is_empty())
        .map(|s| s[s.len() - 1])
        .collect()
}

fn main() {
    println!("Part 1: {}", solve(false));
    println!("Part 2: {}", solve(true));
}
