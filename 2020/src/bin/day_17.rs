use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_17.txt");

fn part1() -> usize {
    let len = INPUT.lines().count() as i32;
    let mut old: HashSet<(i32, i32, i32)> = INPUT
        .lines()
        .enumerate()
        .flat_map(|(y, l)| {
            l.chars()
                .enumerate()
                .filter(|(_x, c)| *c == '#')
                .map(move |(x, _)| (x as i32, y as i32, 0))
        })
        .collect();

    for cycle in 1..=6 {
        let mut new = HashSet::new();
        for z in -cycle..len + cycle {
            for y in -cycle..len + cycle {
                for x in -cycle..len + cycle {
                    let mut count = 0;
                    for dz in -1..=1 {
                        for dy in -1..=1 {
                            for dx in -1..=1 {
                                if dx != 0 || dy != 0 || dz != 0 {
                                    count += old.contains(&(x + dx, y + dy, z + dz)) as usize;
                                }
                            }
                        }
                    }
                    if count == 3 || (old.contains(&(x, y, z)) && count == 2) {
                        new.insert((x, y, z));
                    }
                }
            }
        }
        old = new;
    }
    old.len()
}

fn part2() -> usize {
    let len = INPUT.lines().count() as i32;
    let mut old: HashSet<(i32, i32, i32, i32)> = INPUT
        .lines()
        .enumerate()
        .flat_map(|(y, l)| {
            l.chars()
                .enumerate()
                .filter(|(_x, c)| *c == '#')
                .map(move |(x, _)| (x as i32, y as i32, 0, 0))
        })
        .collect();

    for cycle in 1..=6 {
        let mut new = HashSet::new();
        for w in -cycle..len + cycle {
            for z in -cycle..len + cycle {
                for y in -cycle..len + cycle {
                    for x in -cycle..len + cycle {
                        let mut count = 0;
                        for dw in -1..=1 {
                            for dz in -1..=1 {
                                for dy in -1..=1 {
                                    for dx in -1..=1 {
                                        if dx != 0 || dy != 0 || dz != 0 || dw != 0 {
                                            count += old.contains(&(x + dx, y + dy, z + dz, w + dw))
                                                as usize;
                                        }
                                    }
                                }
                            }
                        }
                        if count == 3 || (old.contains(&(x, y, z, w)) && count == 2) {
                            new.insert((x, y, z, w));
                        }
                    }
                }
            }
        }
        old = new;
    }
    old.len()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
