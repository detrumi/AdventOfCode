use std::collections::HashSet;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_8.txt");

fn parse() -> Vec<Vec<char>> {
    INPUT
        .trim()
        .lines()
        .map(|line| line.chars().collect())
        .collect()
}

fn part1() -> usize {
    let map = parse();
    let freqs: HashSet<char> = map
        .iter()
        .flat_map(|line| line.iter().filter(|&c| *c != '.'))
        .cloned()
        .collect();
    let len = map.len() as i32;
    let mut antinodes = HashSet::new();
    for freq in freqs {
        let mut positions = HashSet::new();
        for y in 0..map.len() {
            for x in 0..map.len() {
                if map[y][x] == freq {
                    positions.insert((x as i32, y as i32));
                }
            }
        }

        for perms in positions.iter().permutations(2) {
            let ((x0, y0), (x1, y1)) = (perms[0], perms[1]);
            let dx = x1 - x0;
            let dy = y1 - y0;
            antinodes.insert((x1 + dx, y1 + dy));
            antinodes.insert((x0 - dx, y0 - dy));
        }
    }
    antinodes
        .iter()
        .filter(|(x, y)| (0..len).contains(x) && (0..len).contains(y))
        .count()
}

fn part2() -> usize {
    let map = parse();
    let freqs: HashSet<char> = map
        .iter()
        .flat_map(|line| line.iter().filter(|&c| *c != '.'))
        .cloned()
        .collect();
    let len = map.len() as i32;
    let mut antinodes = HashSet::new();
    for freq in freqs {
        let mut positions = HashSet::new();
        for y in 0..map.len() {
            for x in 0..map.len() {
                if map[y][x] == freq {
                    positions.insert((x as i32, y as i32));
                    antinodes.insert((x as i32, y as i32));
                }
            }
        }

        for perms in positions.iter().permutations(2) {
            let ((x0, y0), (x1, y1)) = (perms[0], perms[1]);
            let dx = x1 - x0;
            let dy = y1 - y0;
            for n in 1.. {
                let px = x1 + n * dx;
                let py = y1 + n * dy;
                if (0..len).contains(&px) && (0..len).contains(&py) {
                    antinodes.insert((px, py));
                } else {
                    break;
                }
            }
            for n in 1.. {
                let px = x0 - n * dx;
                let py = y0 - n * dy;
                if (0..len).contains(&px) && (0..len).contains(&py) {
                    antinodes.insert((px, py));
                } else {
                    break;
                }
            }
        }
    }
    antinodes.len()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
