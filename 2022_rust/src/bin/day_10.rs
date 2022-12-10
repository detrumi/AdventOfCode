use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_10.txt");

fn part1() -> isize {
    let mut result = 0;
    let mut x = 1;
    let mut cycle = 1;
    let mut check = |x: &isize, cycle: &isize| {
        if (cycle + 20) % 40 == 0 {
            result += cycle * x;
        }
    };
    for line in INPUT.trim().lines() {
        let parts = line.split_ascii_whitespace().collect_vec();
        match parts[0] {
            "noop" => {
                check(&x, &cycle);
                cycle += 1;
            }
            "addx" => {
                check(&x, &cycle);
                cycle += 1;
                check(&x, &cycle);
                x += parts[1].parse::<isize>().unwrap();
                cycle += 1;
            }
            _ => panic!(),
        }
    }
    result
}

fn part2() -> String {
    let mut result = String::new();
    let mut x = 1;
    let mut cycle = 1;
    let mut row = String::from("\n");
    let mut check = |x: &isize, cycle: &isize| {
        let c = ((cycle - 1) % 40) + 1;
        row.push(if matches!(c - x, 0..=2) { '#' } else { '.' });
        if c == 40 {
            result.push_str(&row);
            result.push('\n');
            row.clear();
        }
    };
    for line in INPUT.trim().lines() {
        let parts = line.split_ascii_whitespace().collect_vec();
        match parts[0] {
            "noop" => {
                check(&x, &cycle);
                cycle += 1;
            }
            "addx" => {
                check(&x, &cycle);
                cycle += 1;
                check(&x, &cycle);
                x += parts[1].parse::<isize>().unwrap();
                cycle += 1;
            }
            _ => panic!(),
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
