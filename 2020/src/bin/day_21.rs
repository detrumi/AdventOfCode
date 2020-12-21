use std::collections::{HashMap, HashSet};

const INPUT: &str = include_str!("../../input/day_21.txt");

type Allergent<'a> = &'a str;
type Ingredient<'a> = &'a str;

fn parse<'a>() -> (
    HashMap<Allergent<'a>, HashSet<Ingredient<'a>>>,
    HashMap<Ingredient<'a>, usize>,
) {
    let mut allergent_possibilities: HashMap<_, HashSet<_>> = HashMap::new();
    let mut all_ingredients = HashMap::new();
    for line in INPUT.lines() {
        let parts: Vec<_> = line.split(" (contains ").collect();
        let ingredients: HashSet<_> = parts[0].split_ascii_whitespace().collect();
        for ingredient in &ingredients {
            *all_ingredients.entry(*ingredient).or_default() += 1;
        }
        for allergent in parts[1].trim_end_matches(')').split(", ") {
            allergent_possibilities
                .entry(allergent)
                .and_modify(|s| {
                    *s = s
                        .intersection(&ingredients)
                        .cloned()
                        .collect::<HashSet<Ingredient<'a>>>();
                })
                .or_insert(ingredients.clone());
        }
    }
    (allergent_possibilities, all_ingredients)
}

fn part1<'a>(
    allergent_possibilities: &HashMap<Allergent<'a>, HashSet<Ingredient<'a>>>,
    all_ingredients: &HashMap<Ingredient<'a>, usize>,
) -> usize {
    all_ingredients
        .iter()
        .filter(|&(i, _)| (allergent_possibilities.values().all(|p| !p.contains(i))))
        .map(|(_, count)| count)
        .sum()
}

fn part2<'a>(
    allergent_possibilities: &mut HashMap<Allergent<'a>, HashSet<Ingredient<'a>>>,
) -> String {
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
    let (mut allergent_possibilities, all_ingredients) = parse();
    println!(
        "Part 1: {}",
        part1(&allergent_possibilities, &all_ingredients)
    );
    println!("Part 2: {}", part2(&mut allergent_possibilities));
}
