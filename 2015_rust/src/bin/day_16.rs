use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_16.txt");

fn parse<'a>() -> impl Iterator<Item = HashMap<&'a str, u32>> {
    INPUT.lines().map(|line| {
        let colon_index = line.match_indices(':').next().unwrap().0;
        line.split_at(colon_index + 2)
            .1
            .split(", ")
            .map(|s| {
                let kv: Vec<_> = s.split(": ").collect();
                (kv[0], kv[1].parse().unwrap())
            })
            .collect()
    })
}

fn message() -> HashMap<&'static str, u32> {
    [
        ("children", 3),
        ("cats", 7),
        ("samoyeds", 2),
        ("pomeranians", 3),
        ("akitas", 0),
        ("vizslas", 0),
        ("goldfish", 5),
        ("trees", 3),
        ("cars", 2),
        ("perfumes", 1),
    ]
    .iter()
    .cloned()
    .collect()
}

fn part1() -> usize {
    let message = message();
    for (sue, number) in parse().zip(1..) {
        if sue
            .iter()
            .all(|(key, value)| message.get(key) == Some(value))
        {
            return number;
        }
    }
    unreachable!()
}

fn part2() -> usize {
    let message = message();
    for (sue, number) in parse().zip(1..) {
        if sue.iter().all(|(key, value)| match *key {
            "cats" | "trees" => message.get(key) < Some(value),
            "pomeranians" | "goldfish" => message.get(key) > Some(value),
            _ => message.get(key) == Some(value),
        }) {
            return number;
        }
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
