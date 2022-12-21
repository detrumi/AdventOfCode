use std::collections::HashMap;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_21.txt");

fn parse() -> HashMap<String, Vec<String>> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let (monkey, right) = line.split_once(": ").unwrap();
            let parts = right
                .split_ascii_whitespace()
                .map(|s| s.to_string())
                .collect_vec();
            (monkey.to_string(), parts)
        })
        .collect()
}

fn part1() -> isize {
    solve1("root".to_string(), &parse())
}

fn solve1(monkey: String, input: &HashMap<String, Vec<String>>) -> isize {
    let parts = &input[&monkey];
    if parts.len() == 1 {
        return parts[0].parse().unwrap();
    }
    match parts[1].as_str() {
        "+" => solve1(parts[0].clone(), input) + solve1(parts[2].clone(), input),
        "-" => solve1(parts[0].clone(), input) - solve1(parts[2].clone(), input),
        "*" => solve1(parts[0].clone(), input) * solve1(parts[2].clone(), input),
        "/" => solve1(parts[0].clone(), input) / solve1(parts[2].clone(), input),
        _ => panic!(),
    }
}

fn part2() -> String {
    let input = parse();
    let parts = input["root"].clone();
    let a = solve2(parts[0].clone(), &input)
        .map(|n| n.to_string())
        .unwrap_or_else(|e| e);
    let b = solve2(parts[2].clone(), &input)
        .map(|n| n.to_string())
        .unwrap_or_else(|e| e);
    format!("{a} == {b}")
}

fn solve2(monkey: String, input: &HashMap<String, Vec<String>>) -> Result<isize, String> {
    let parts = &input[&monkey];
    if parts.len() == 1 {
        if monkey == "humn" {
            return Err("X".to_string());
        }
        return Ok(parts[0].parse().unwrap());
    }
    let l = solve2(parts[0].clone(), input);
    let r = solve2(parts[2].clone(), input);
    if l.is_ok() && r.is_ok() {
        match parts[1].as_str() {
            "+" => Ok(l.unwrap() + r.unwrap()),
            "-" => Ok(l.unwrap() - r.unwrap()),
            "*" => Ok(l.unwrap() * r.unwrap()),
            "/" => Ok(l.unwrap() / r.unwrap()),
            _ => panic!(),
        }
    } else {
        let l = l.map(|n| n.to_string()).unwrap_or_else(|e| e);
        let r = r.map(|n| n.to_string()).unwrap_or_else(|e| e);
        match parts[1].as_str() {
            "+" => Err(format!("({l} + {r})")),
            "-" => Err(format!("({l} - {r})")),
            "*" => Err(format!("({l} * {r})")),
            "/" => Err(format!("({l} / {r})")),
            _ => panic!(),
        }
    }
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
