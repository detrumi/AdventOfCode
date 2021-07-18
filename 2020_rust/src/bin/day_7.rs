use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_7.txt");
const SHINY_GOLD: &str = "shiny gold";

fn parse() -> HashMap<String, Vec<(usize, String)>> {
    let mut pairs = HashMap::new();
    for line in INPUT.lines() {
        let parts: Vec<_> = line.split(" contain ").collect();
        let color = parts[0].trim_end_matches(" bags").to_string();
        let bags: Vec<_> = parts[1]
            .split(", ")
            .map(|bag| {
                if bag == "no other bags." {
                    return (0, String::new());
                }
                let parts = bag.split_ascii_whitespace().collect::<Vec<_>>();
                let amount = parts[0].parse::<usize>().unwrap();
                let color = parts[1..3].join(" ").to_string();
                (amount, color)
            })
            .filter(|(amount, _)| *amount > 0)
            .collect();
        pairs.insert(color, bags);
    }
    pairs
}

fn has_shiny_gold(color: String, pairs: &HashMap<String, Vec<(usize, String)>>) -> bool {
    pairs[&color]
        .iter()
        .any(|(_, bag)| *bag == SHINY_GOLD || has_shiny_gold(bag.clone(), pairs))
}

fn count_shiny_golds(color: String, pairs: &HashMap<String, Vec<(usize, String)>>) -> usize {
    pairs[&color]
        .iter()
        .map(|(amount, bag)| amount + amount * count_shiny_golds(bag.clone(), pairs))
        .sum()
}

fn part1() -> usize {
    let pairs = parse();
    pairs
        .keys()
        .filter(|&color| has_shiny_gold(color.clone(), &pairs))
        .count()
}

fn part2() -> usize {
    count_shiny_golds(SHINY_GOLD.to_string(), &parse())
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
