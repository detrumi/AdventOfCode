use std::cmp::Ordering;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_13.txt");

fn parse_line(line: &str) -> Vec<String> {
    line.replace(",", " ")
        .replace("[", " [ ")
        .replace("]", " ] ")
        .split_ascii_whitespace()
        .map(|s| s.to_string())
        .collect_vec()
}

fn is_ordered(mut left: Vec<String>, mut right: Vec<String>) -> bool {
    let (mut l, mut r) = (0, 0);
    loop {
        if l >= left.len() {
            return true;
        }
        if r >= right.len() {
            return false;
        }
        match (left[l].as_ref(), right[r].as_ref()) {
            ("[", "[") => (),
            ("]", "]") => (),
            ("[", _) => {
                right.insert(r + 1, "]".into());
                right.insert(r, "[".into());
                continue;
            }
            (_, "[") => {
                left.insert(l + 1, "]".into());
                left.insert(l, "[".into());
                continue;
            }

            (_, "]") => {
                return false;
            }
            ("]", _) => {
                return true;
            }
            (_, _) => {
                let num_l: usize = left[l].parse().unwrap();
                let num_r: usize = right[r].parse().unwrap();

                if num_l > num_r {
                    return false;
                } else if num_l < num_r {
                    return true;
                }
            }
        }
        l += 1;
        r += 1;
    }
}

fn part1() -> usize {
    let mut result = 0;
    for (section, index) in INPUT.trim().split("\n\n").zip(1..) {
        let mut lines = section.lines().map(parse_line);
        if is_ordered(lines.next().unwrap(), lines.next().unwrap()) {
            result += index;
        }
    }
    result
}

fn part2() -> usize {
    let divider_1 = parse_line("[[2]]");
    let divider_2 = parse_line("[[6]]");
    let mut lines = INPUT
        .trim()
        .lines()
        .filter(|line| !line.is_empty())
        .map(parse_line)
        .chain(vec![divider_1.clone(), divider_2.clone()])
        .collect_vec();
    lines.sort_by(|a, b| {
        if is_ordered(a.clone(), b.clone()) {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    });
    let a = lines.iter().position(|v| v == &divider_1).unwrap() + 1;
    let b = lines.iter().position(|v| v == &divider_2).unwrap() + 1;
    a * b
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
