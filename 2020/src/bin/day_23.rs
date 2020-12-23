use std::collections::VecDeque;

const INPUT: &str = include_str!("../../input/day_23.txt");

fn solve(mut cups: VecDeque<u32>, rounds: usize) -> VecDeque<u32> {
    let len = cups.len() as u32;

    let mut old_first = cups[0];
    for _move in 0..rounds {
        let mut target = old_first - 1;
        if target == 0 {
            target = len;
        }

        cups.rotate_left(1);
        let mut pickup = vec![];
        for _ in 0..3 {
            pickup.push(cups.pop_front().unwrap());
        }

        while pickup.contains(&target) {
            target -= 1;
            if target == 0 {
                target = len;
            }
        }

        cups.rotate_left(cups.iter().position(|&c| c == target).unwrap() + 1);
        for _ in 0..3 {
            cups.push_front(pickup.pop().unwrap());
        }
        cups.rotate_right(1);

        cups.rotate_left(cups.iter().position(|&c| c == old_first).unwrap() + 1);
        old_first = cups[0];
    }

    cups.rotate_left(cups.iter().position(|&c| c == 1).unwrap() + 1);
    cups
}

fn part1(cups: VecDeque<u32>) -> String {
    solve(cups, 100)
        .iter()
        .map(|&n| std::char::from_digit(n, 10).unwrap())
        .collect()
}

fn part2(mut cups: VecDeque<u32>) -> u64 {
    cups.extend(cups.len() as u32 + 1..=1_000_000);
    cups = solve(cups.clone(), 10_000_000);
    cups[0] as u64 * cups[1] as u64
}

fn main() {
    let cups: VecDeque<_> = INPUT
        .chars()
        .map(|c| char::to_digit(c, 10).unwrap())
        .collect();

    println!("Part 1: {}", part1(cups.clone()));
    println!("Part 2: {}", part2(cups));
}
