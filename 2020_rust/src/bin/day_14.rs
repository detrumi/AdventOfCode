#![feature(str_split_once)]

use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_14.txt");

fn parse() -> impl Iterator<Item = (Vec<char>, Vec<(usize, usize)>)> {
    INPUT.split("mask = ").skip(1).map(|chunk| {
        let (mask, lines) = chunk.split_once("\n").unwrap();
        let mask = mask.chars().collect();
        let values = lines
            .lines()
            .map(|line| {
                let parts: Vec<_> = line.split(" = ").collect();
                let address: usize = parts[0][4..parts[0].len() - 1].parse().unwrap();
                let value = parts[1].parse::<usize>().unwrap();
                (address, value)
            })
            .collect();
        (mask, values)
    })
}

fn binary(value: usize) -> Vec<char> {
    let binary_string = format!("{:b}", value);
    ("0".repeat(36 - binary_string.len()) + &binary_string)
        .chars()
        .collect()
}

fn part1() -> usize {
    let mut memory: HashMap<usize, usize> = HashMap::new();
    for (mask, values) in parse() {
        for (address, value) in values {
            let result: Vec<_> = mask
                .iter()
                .zip(binary(value))
                .map(|(&m, c)| if m == 'X' { c } else { m })
                .collect();
            let decimal = usize::from_str_radix(&result.iter().collect::<String>(), 2).unwrap();
            memory.insert(address, decimal);
        }
    }
    memory.values().sum()
}

fn all_values(addresses: &Vec<char>, index: usize) -> Vec<Vec<char>> {
    let mut results = if index == 0 {
        vec![vec![]]
    } else {
        all_values(addresses, index - 1)
    };
    match addresses[index] {
        'X' => {
            let mut new_results = vec![];
            for result in results {
                new_results.push(result.clone());
                new_results.last_mut().unwrap().push('0');
                new_results.push(result);
                new_results.last_mut().unwrap().push('1');
            }
            new_results
        }
        b => {
            for result in &mut results {
                result.push(b);
            }
            results
        }
    }
}

fn part2() -> usize {
    let mut memory: HashMap<usize, usize> = HashMap::new();
    for (mask, values) in parse() {
        for (address, value) in values {
            let floating_mask: Vec<_> = mask
                .iter()
                .zip(binary(address))
                .map(|(&m, c)| match m {
                    '0' => c,
                    '1' => m,
                    _ => 'X',
                })
                .collect();
            for result in all_values(&floating_mask, floating_mask.len() - 1) {
                let result = usize::from_str_radix(&result.iter().collect::<String>(), 2).unwrap();
                memory.insert(result, value);
            }
        }
    }
    memory.values().sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
