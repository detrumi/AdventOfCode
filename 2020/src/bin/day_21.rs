use std::collections::{HashMap, HashSet};

const INPUT: &str = include_str!("../../input/day_21.txt");

fn part1() -> usize {
    let mut allergent_possibilities: HashMap<&str, HashSet<&str>> = HashMap::new();
    let mut all_ingredients: HashMap<&str, usize> = HashMap::new();
    for line in INPUT.lines() {
        let parts: Vec<_> = line.split(" (contains ").collect();
        let ingredients: HashSet<_> = parts[0].split_ascii_whitespace().collect();
        let allergens: HashSet<_> = parts[1].trim_end_matches(')').split(", ").collect();
        for ingredient in &ingredients {
            *all_ingredients.entry(ingredient).or_default() += 1;
        }
        for allergent in allergens {
            if allergent_possibilities.contains_key(allergent) {
                let value = allergent_possibilities[allergent]
                    .intersection(&ingredients)
                    .cloned()
                    .collect();
                allergent_possibilities.insert(allergent, value);
            } else {
                allergent_possibilities.insert(allergent, ingredients.clone());
            }
        }
    }

    all_ingredients
        .iter()
        .filter(|&(i, _)| (allergent_possibilities.values().all(|p| !p.contains(i))))
        .map(|(_, count)| count)
        .sum()
}

fn part2() -> String {
    let mut allergent_possibilities: HashMap<&str, HashSet<&str>> = HashMap::new();
    let mut all_ingredients: HashMap<&str, usize> = HashMap::new();
    for line in INPUT.lines() {
        let parts: Vec<_> = line.split(" (contains ").collect();
        let ingredients: HashSet<_> = parts[0].split_ascii_whitespace().collect();
        let allergens: HashSet<_> = parts[1].trim_end_matches(')').split(", ").collect();
        for ingredient in &ingredients {
            *all_ingredients.entry(ingredient).or_default() += 1;
        }
        for allergent in allergens {
            if allergent_possibilities.contains_key(allergent) {
                let value = allergent_possibilities[allergent]
                    .intersection(&ingredients)
                    .cloned()
                    .collect();
                allergent_possibilities.insert(allergent, value);
            } else {
                allergent_possibilities.insert(allergent, ingredients.clone());
            }
        }
    }

    let mut mapping = vec![];
    while let Some((allergent, ingredient)) = allergent_possibilities
        .iter()
        .find(|(_, ingredients)| ingredients.len() == 1)
        .map(|(a, ingredients)| (a.to_owned(), ingredients.iter().next().unwrap().to_owned()))
    {
        mapping.push((allergent, ingredient));
        allergent_possibilities.remove(allergent);
        for (_, ingredients) in allergent_possibilities.iter_mut() {
            ingredients.remove(ingredient);
        }
    }
    mapping.sort_by_key(|(a, _)| *a);
    itertools::join(mapping.iter().map(|(_, i)| i), ",")
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
