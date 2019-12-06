use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_6.txt").unwrap();
    let lines: Vec<Vec<String>> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap().split(")").map(|s| s.to_string()).collect())
        .collect();

    let mut orbits: HashMap<String, Vec<String>> = HashMap::new();
    for line in &lines {
        orbits
            .entry(line[1].clone())
            .or_default()
            .push(line[0].clone());
    }

    let mut num_orbits = 0;
    for orbit in orbits.keys() {
        num_orbits += count(orbit.to_string(), &orbits, &mut HashSet::new());
    }
    eprintln!("Part 1 = {:?}", num_orbits);

    let start = lines
        .iter()
        .find(|l| l[1] == "YOU")
        .unwrap()
        .get(0)
        .unwrap();
    let end = lines
        .iter()
        .find(|l| l[1] == "SAN")
        .unwrap()
        .get(0)
        .unwrap();

    let mut nested: HashMap<String, HashSet<String>> = HashMap::new();
    for o in orbits.keys() {
        let mut hash_set = HashSet::new();
        nest(o.clone(), &orbits, &mut hash_set);
        nested.insert(o.to_string(), hash_set);
    }

    let mut results = vec![];
    let o1: HashSet<&String> = nested["YOU"].iter().collect();
    let o2: HashSet<&String> = nested["SAN"].iter().collect();
    for intersection in o1.intersection(&o2) {
        let mut start2 = start;
        let mut end2 = end;

        let mut steps = 0;
        while start2 != *intersection {
            start2 = orbits[start2].get(0).unwrap();
            steps += 1;
        }
        while end2 != *intersection {
            end2 = orbits[end2].get(0).unwrap();
            steps += 1;
        }
        results.push(steps);
    }
    eprintln!("Part 2 = {:?}", results.iter().min().unwrap());
}

fn nest(s: String, orbits: &HashMap<String, Vec<String>>, result: &mut HashSet<String>) {
    if orbits.get(&s).is_none() {
        return;
    }
    for o in &orbits[&s] {
        result.insert(o.clone());
        nest(o.to_string(), orbits, result);
    }
}

fn count(s: String, orbits: &HashMap<String, Vec<String>>, found: &mut HashSet<String>) -> usize {
    let mut result = 0;
    // eprintln!("s = {:?}, found = {:?}", s, found);
    found.insert(s.clone());
    if let Some(inner) = orbits.get(&s) {
        for o in inner {
            result += 1;
            if !found.contains(o) {
                result += count(o.to_string(), orbits, found);
            }
        }
    }
    result
}
