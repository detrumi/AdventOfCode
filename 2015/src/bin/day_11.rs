use itertools::Itertools;
use std::char;

const INPUT: &str = "vzbxkghb";

fn char_increment(c: char) -> char {
    char::from_u32(c as u32 + 1).unwrap()
}

fn increment(password: &mut Vec<char>) {
    for i in (0..password.len()).rev() {
        if password[i] == 'z' {
            password[i] = 'a';
        } else {
            password[i] = char_increment(password[i]);
            return;
        }
    }
}

fn is_secure(password: &[char]) -> bool {
    if password.iter().any(|&c| c == 'i' || c == 'o' || c == 'l') {
        return false;
    }

    let mut has_straight = false;
    for (&a, &b, &c) in password.iter().tuple_windows() {
        if char_increment(a) == b && char_increment(b) == c {
            has_straight = true;
        }
    }

    let mut pairs = 0;
    let mut i = 1;
    while i < password.len() {
        if password[i] == password[i - 1] {
            pairs += 1;
            i += 2;
        } else {
            i += 1;
        }
    }

    has_straight && pairs >= 2
}

fn part1() -> String {
    let mut password = INPUT.chars().collect();
    increment(&mut password);
    while !is_secure(&password) {
        increment(&mut password);
    }
    password.iter().collect()
}

fn part2() -> String {
    let mut password = part1().chars().collect();
    increment(&mut password);
    while !is_secure(&password) {
        increment(&mut password);
    }
    password.iter().collect()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
