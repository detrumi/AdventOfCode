use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_18.txt");
const SIZE: isize = 50;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
enum Tile {
    Open,
    Trees,
    Lumberyard,
}

impl From<char> for Tile {
    fn from(c: char) -> Self {
        match c {
            '.' => Tile::Open,
            '|' => Tile::Trees,
            '#' => Tile::Lumberyard,
            _ => panic!(),
        }
    }
}

fn parse() -> Vec<Tile> {
    INPUT
        .lines()
        .flat_map(|line| line.chars().map(Tile::from))
        .collect()
}

fn updated_tile(tile: Tile, trees: i32, lumberyards: i32) -> Tile {
    match tile {
        Tile::Open => {
            if trees >= 3 {
                Tile::Trees
            } else {
                Tile::Open
            }
        }
        Tile::Trees => {
            if lumberyards >= 3 {
                Tile::Lumberyard
            } else {
                Tile::Trees
            }
        }
        Tile::Lumberyard => {
            if lumberyards >= 1 && trees >= 1 {
                Tile::Lumberyard
            } else {
                Tile::Open
            }
        }
    }
}

fn solve(target_iterations: usize) -> usize {
    let mut old = parse();
    let mut seen = HashMap::new();
    let mut iteration = 0;
    while iteration < target_iterations {
        let mut new = Vec::with_capacity(old.len());
        for y in 0..SIZE {
            for x in 0..SIZE {
                let mut trees = 0;
                let mut lumberyards = 0;
                for dy in (-1..=1).filter(|dy| y + dy >= 0 && y + dy < SIZE) {
                    for dx in (-1..=1).filter(|dx| x + dx >= 0 && x + dx < SIZE) {
                        if dx == 0 && dy == 0 {
                            continue;
                        }

                        let n = (y + dy) * SIZE + x + dx;
                        match old[n as usize] {
                            Tile::Open => (),
                            Tile::Trees => trees += 1,
                            Tile::Lumberyard => lumberyards += 1,
                        }
                    }
                }
                let new_tile = updated_tile(old[(SIZE * y + x) as usize], trees, lumberyards);
                new.push(new_tile);
            }
        }
        iteration += if let Some(previous_iteration) = seen.insert(new.clone(), iteration) {
            let duration = iteration - previous_iteration;
            let laps = (target_iterations - iteration) / duration;
            laps * duration + 1
        } else {
            1
        };
        old = new;
    }
    let trees = old.iter().filter(|t| **t == Tile::Trees).count();
    let lumberyards = old.iter().filter(|t| **t == Tile::Lumberyard).count();
    trees * lumberyards
}

fn main() {
    println!("Part 1: {}", solve(10));
    println!("Part 2: {}", solve(1_000_000_000));
}
