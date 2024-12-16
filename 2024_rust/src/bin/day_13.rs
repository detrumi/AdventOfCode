use std::isize;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_13.txt");

struct Config {
    pub x_a: isize,
    pub y_a: isize,
    pub x_b: isize,
    pub y_b: isize,
    pub x_price: isize,
    pub y_price: isize,
}

fn parse() -> Vec<Config> {
    INPUT
        .trim()
        .split("\n\n")
        .map(|part| {
            let lines = part
                .lines()
                .map(|l| {
                    l.replace("+", " ")
                        .replace("=", " ")
                        .replace(",", " ")
                        .split_ascii_whitespace()
                        .map(|s| s.to_string())
                        .collect_vec()
                })
                .collect_vec();
            Config {
                x_a: lines[0][3].parse().unwrap(),
                y_a: lines[0][5].parse().unwrap(),
                x_b: lines[1][3].parse().unwrap(),
                y_b: lines[1][5].parse().unwrap(),
                x_price: lines[2][2].parse().unwrap(),
                y_price: lines[2][4].parse().unwrap(),
            }
        })
        .collect_vec()
}

fn part1() -> isize {
    let mut result = 0;
    for Config {
        x_a,
        y_a,
        x_b,
        y_b,
        x_price,
        y_price,
    } in parse()
    {
        let (mut x, mut y) = (0, 0);
        let mut best = isize::MAX;
        for a_presses in 0..=100 {
            if x > x_price || y > y_price {
                break;
            }
            let b_presses = (x_price - x) / x_b;
            if (x_price - x) % x_b == 0
                && y + y_b * b_presses == y_price
                && (y_price - y) / y_b <= 100
            {
                best = best.min(3 * a_presses + (y_price - y) / y_b);
            }
            x += x_a;
            y += y_a;
        }
        if best < isize::MAX {
            result += best;
        }
    }
    result
}

fn part2() -> isize {
    let mut result = 0;
    for Config {
        x_a,
        y_a,
        x_b,
        y_b,
        mut x_price,
        mut y_price,
    } in parse()
    {
        x_price += 10000000000000;
        y_price += 10000000000000;
        let b = (x_a * y_price - y_a * x_price) / (y_b * x_a - x_b * y_a);
        let a = (x_price - b * x_b) / x_a;
        if a * x_a + b * x_b == x_price && a * y_a + b * y_b == y_price {
            result += 3 * a + b;
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
