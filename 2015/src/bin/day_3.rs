use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_3.txt");

fn next(c: char, (x, y): &mut (i32, i32)) {
    match c {
        '^' => *y -= 1,
        '>' => *x += 1,
        'v' => *y += 1,
        '<' => *x -= 1,
        _ => (),
    }
}

fn part1() -> usize {
    let mut visited: HashSet<(i32, i32)> = HashSet::new();

    let mut pos = (0, 0);
    visited.insert(pos);

    for c in INPUT.chars() {
        next(c, &mut pos);
        visited.insert(pos);
    }
    visited.len()
}

fn part2() -> usize {
    let mut visited: HashSet<(i32, i32)> = HashSet::new();

    let mut pos1 = (0, 0);
    let mut pos2 = (0, 0);
    visited.insert(pos1);

    for (i, c) in INPUT.char_indices() {
        let pos = if i % 2 == 0 { &mut pos1 } else { &mut pos2 };
        next(c, pos);
        visited.insert(*pos);
    }
    visited.len()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
