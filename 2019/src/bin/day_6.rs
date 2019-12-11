use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_6.txt").unwrap();
    let lines: Vec<Vec<String>> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap().split(')').map(|s| s.to_string()).collect())
        .collect();

    let mut orbits: HashMap<String, String> = HashMap::new();
    for line in &lines {
        orbits.insert(line[1].clone(), line[0].clone());
    }

    println!("Part 1 = {}", part_1(&orbits));
    println!("Part 2 = {}", part_2(&lines, &orbits).unwrap());
}

fn part_1(orbits: &HashMap<String, String>) -> usize {
    let mut num_orbits = 0;
    for orbit in orbits.keys() {
        num_orbits += count(orbit.to_string(), &orbits, &mut HashSet::new());
    }
    num_orbits
}

fn part_2(lines: &[Vec<String>], orbits: &HashMap<String, String>) -> Option<usize> {
    let start = lines.iter().find(|l| l[1] == "YOU")?.first()?;
    let end = lines.iter().find(|l| l[1] == "SAN")?.first()?;

    let mut nested: HashMap<String, HashSet<String>> = HashMap::new();
    for o in orbits.keys() {
        let mut hash_set = HashSet::new();
        nest(o.clone(), &orbits, &mut hash_set);
        nested.insert(o.to_string(), hash_set);
    }

    nested["YOU"]
        .intersection(&nested["SAN"])
        .map(|i| path_length(i, start, &orbits) + path_length(i, end, &orbits))
        .min()
}

fn path_length<'a>(
    intersection: &str,
    mut current: &'a str,
    orbits: &'a HashMap<String, String>,
) -> usize {
    let mut steps = 0;
    while current != intersection {
        if let Some(o) = orbits.get(current) {
            current = o;
            steps += 1;
        } else {
            break;
        }
    }
    steps
}

fn nest(s: String, orbits: &HashMap<String, String>, result: &mut HashSet<String>) {
    if let Some(o) = orbits.get(&s) {
        result.insert(o.clone());
        nest(o.to_string(), orbits, result);
    }
}

fn count(s: String, orbits: &HashMap<String, String>, found: &mut HashSet<String>) -> usize {
    let mut result = 0;
    found.insert(s.clone());
    if let Some(o) = orbits.get(&s) {
        result += 1;
        if !found.contains(o) {
            result += count(o.to_string(), orbits, found);
        }
    }
    result
}
