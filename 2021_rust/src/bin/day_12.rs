use std::collections::{HashMap, HashSet};

const INPUT: &str = include_str!("../../input/day_12.txt");

fn parse() -> HashMap<String, Vec<String>> {
    let input: Vec<(String, String)> = INPUT
        .trim()
        .lines()
        .map(|line| {
            let (a, b) = line.split_once('-').unwrap();
            (a.to_string(), b.to_string())
        })
        .collect();

    let mut paths: HashMap<String, Vec<String>> = HashMap::new();
    for (a, b) in input {
        paths.entry(a.clone()).or_default().push(b.clone());
        paths.entry(b).or_default().push(a);
    }
    paths
}

fn is_big_cave(s: &str) -> bool {
    s.chars().next().unwrap().is_ascii_uppercase()
}

fn part1() -> usize {
    let paths = parse();
    let mut routes = vec![vec!["start".to_string()]];
    let mut result = 0;
    while let Some(route) = routes.pop() {
        for neighbor in &paths[route.last().unwrap()] {
            if neighbor == "end" {
                result += 1;
            } else if is_big_cave(&neighbor) || !route.contains(&neighbor) {
                let mut new_route = route.clone();
                new_route.push(neighbor.clone());
                routes.push(new_route);
            }
        }
    }
    result
}

fn part2() -> usize {
    let paths = parse();
    let mut unique_routes = HashSet::new();
    for cave in paths.keys() {
        if is_big_cave(&cave) || cave == "start" || cave == "end" {
            continue;
        }

        let mut routes = vec![vec!["start".to_string()]];
        while let Some(route) = routes.pop() {
            for neighbor in &paths[route.last().unwrap()] {
                if neighbor == "end" {
                    unique_routes.insert(route.clone());
                } else if is_big_cave(&neighbor)
                    || !route.contains(&neighbor)
                    || (neighbor == cave && route.iter().filter(|c| *c == cave).count() == 1)
                {
                    let mut new_route = route.clone();
                    new_route.push(neighbor.clone());
                    routes.push(new_route);
                }
            }
        }
    }
    unique_routes.len()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
