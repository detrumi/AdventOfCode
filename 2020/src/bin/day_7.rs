use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_7.txt");

type Color = Vec<String>;

fn has_shiny_gold(color: Color, pairs: &HashMap<Color, Vec<Color>>) -> bool {
    if pairs
        .get(&color)
        .unwrap()
        .iter()
        .any(|v| *v == shiny_gold())
    {
        return true;
    }
    for bag in pairs.get(&color).unwrap() {
        if has_shiny_gold(bag.to_vec(), pairs) {
            return true;
        }
    }
    false
}

fn count_shiny_golds(color: Color, pairs: &HashMap<Color, Vec<(usize, Color)>>) -> usize {
    pairs
        .get(&color)
        .unwrap()
        .iter()
        .map(|(amount, bag)| {
            if *amount == 0 {
                return 0;
            }
            amount + amount * count_shiny_golds(bag.clone(), pairs)
        })
        .sum()
}

fn shiny_gold() -> Color {
    vec!["shiny".into(), "gold".into()]
}

fn part1() -> usize {
    let mut pairs = HashMap::new();
    for line in INPUT.lines() {
        let parts: Vec<_> = line.split(" contain ").collect();
        let color = parts[0]
            .split_ascii_whitespace()
            .map(|s| s.to_string())
            .take(2)
            .collect::<Color>();
        let mut bags: Vec<Color> = parts[1]
            .split(", ")
            .map(|bag| {
                if bag == "no other bags." {
                    return vec![];
                }
                let parts = bag.split_ascii_whitespace().collect::<Vec<_>>();
                let color = parts[1..3]
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>();
                color
            })
            .collect();
        if bags[0].is_empty() {
            bags = vec![];
        }
        pairs.insert(color, bags);
    }

    let mut result = 0;
    for color in pairs.keys() {
        if has_shiny_gold(color.clone(), &pairs) {
            result += 1;
        }
    }

    result
}

fn part2() -> usize {
    let mut pairs = HashMap::new();
    for line in INPUT.lines() {
        let parts: Vec<_> = line.split(" contain ").collect();
        let color = parts[0]
            .split_ascii_whitespace()
            .map(|s| s.to_string())
            .take(2)
            .collect::<Vec<_>>();
        let bags: Vec<_> = parts[1]
            .split(", ")
            .map(|bag| {
                if bag == "no other bags." {
                    return (0, vec![]);
                }
                let parts = bag.split_ascii_whitespace().collect::<Vec<_>>();
                let color = parts[1..3]
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>();
                let amount = parts
                    .iter()
                    .filter_map(|w| w.parse::<usize>().ok())
                    .next()
                    .unwrap();
                (amount, color)
            })
            .collect();
        pairs.insert(color, bags);
    }
    count_shiny_golds(shiny_gold(), &pairs)
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
