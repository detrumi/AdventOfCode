use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_24.txt");

fn flips() -> HashSet<(i32, i32)> {
    let mut flipped = HashSet::new();
    for line in INPUT.lines() {
        let (mut x, mut y) = (0, 0);
        let mut chars = line.chars();
        while let Some(a) = chars.next() {
            match a {
                'e' => x += 1,
                'w' => x -= 1,
                'n' => match chars.next().unwrap() {
                    'w' => y -= 1,
                    'e' => {
                        x += 1;
                        y -= 1;
                    }
                    _ => unreachable!(),
                },
                's' => match chars.next().unwrap() {
                    'e' => y += 1,
                    'w' => {
                        x -= 1;
                        y += 1;
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }
        if flipped.contains(&(x, y)) {
            flipped.remove(&(x, y));
        } else {
            flipped.insert((x, y));
        }
    }
    flipped
}

fn part2(mut flipped: HashSet<(i32, i32)>) -> usize {
    for _ in 0..100 {
        let mut new = HashSet::new();
        for x in -100..100 {
            for y in -100..100 {
                let mut count = 0;
                for neighbor in &[(1, 0), (0, 1), (-1, 1), (-1, 0), (0, -1), (1, -1)] {
                    if flipped.contains(&(x + neighbor.0, y + neighbor.1)) {
                        count += 1;
                    }
                }
                if flipped.contains(&(x, y)) {
                    if count == 1 || count == 2 {
                        new.insert((x, y));
                    }
                } else {
                    if count == 2 {
                        new.insert((x, y));
                    }
                }
            }
        }
        flipped = new;
    }
    flipped.len()
}

fn main() {
    let flipped = flips();
    println!("Part 1: {}", flipped.len());
    println!("Part 2: {}", part2(flipped));
}
