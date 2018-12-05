use std::fs::File;
use std::io::{self, BufRead};
use std::char;

fn main() {
    let file = File::open("input/day_5.txt").unwrap();
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .collect();

    let mut chars: Vec<char> = lines[0].clone().chars().collect();
    let mut i = 0;
    while i < chars.len() - 1 {
        if chars[i].to_lowercase().to_string() == chars[i + 1].to_lowercase().to_string()
            && (chars[i].is_lowercase() != chars[i + 1].is_lowercase()) {
                chars.remove(i);
                chars.remove(i);
                if i > 0 { i -= 1; }
            } else {
                i += 1;
            }
    }
    println!("Part 1: {}", chars.len());

    let mut best = std::usize::MAX;
    for i in 0..26 {
        let mut chars: Vec<char> = lines[0].clone().chars().collect();
        let letter = char::from_u32('a' as u32 + i).unwrap();
        chars.retain(|c| c.to_lowercase().next().unwrap() != letter);

        let mut i = 0;
        while i < chars.len() - 1 {
            if chars[i].to_lowercase().to_string() == chars[i + 1].to_lowercase().to_string()
                && (chars[i].is_lowercase() != chars[i + 1].is_lowercase()) {
                    chars.remove(i);
                    chars.remove(i);
                    if i > 0 { i -= 1; }
                } else {
                    i += 1;
                }
        }
        best = best.min(chars.len());
    }
    println!("Part 2: {}", best);
}
