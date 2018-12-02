use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead};
use std::char;

fn main() {
    let file = File::open("input/day_2.txt").unwrap();
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .collect();

    part_1(lines.clone());
    part_2(lines);
}

fn part_1(lines: Vec<String>) {
    let mut twos = 0;
    let mut threes = 0;
    for line in lines {
        let mut two = false;
        let mut three = false;
        for i in 0..26 {
            let letter = char::from_u32('a' as u32 + i).unwrap();
            let count = line.chars().filter(|c| c == &letter).count();
            if count == 2 { two = true; }
            if count == 3 { three = true; }
        }
        if two { twos += 1; }
        if three { threes += 1; }
    }
    println!("Part 1: {}", twos * threes);
}

fn part_2(lines: Vec<String>) {
    let mut equalities: Vec<HashSet<String>> = vec![HashSet::new(); 26];
    for line in lines {
        for i in 0..line.len() {
            let mut v: Vec<char> = line.chars().collect();
            v[i] = '_';
            let s: String = v.iter().collect();
            if equalities[i].contains(&s) {
                println!("Part 2: {}", s.replace("_", ""));
                return;
            }
            equalities[i].insert(s);
        }
    }
}
