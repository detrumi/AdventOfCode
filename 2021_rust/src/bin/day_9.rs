use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_9.txt");

struct Map {
    pub width: usize,
    pub height: usize,
    pub tiles: Vec<usize>,
}

impl Map {
    pub fn parse() -> Self {
        let lines: Vec<_> = INPUT.trim().lines().collect();
        let width = lines[0].len();
        let height = lines.len();
        let tiles = lines
            .iter()
            .flat_map(|line| {
                line.chars()
                    .map(|c| char::to_digit(c, 10).unwrap() as usize)
            })
            .collect();

        Self {
            width,
            height,
            tiles,
        }
    }

    pub fn neighbors(&self, pos: usize) -> Vec<usize> {
        let x = pos % self.width;
        let y = pos / self.width;
        let mut result = vec![];
        if x > 0 {
            result.push(pos - 1)
        }
        if x + 1 < self.width {
            result.push(pos + 1)
        }
        if y > 0 {
            result.push(pos - self.width)
        }
        if y + 1 < self.height {
            result.push(pos + self.width)
        }
        result
    }
}

fn part1() -> usize {
    let map = Map::parse();
    (0..map.tiles.len())
        .filter(|pos| {
            map.neighbors(*pos)
                .iter()
                .all(|n| map.tiles[*pos] < map.tiles[*n])
        })
        .map(|pos| 1 + map.tiles[pos])
        .sum()
}

fn part2() -> usize {
    let map = Map::parse();
    let mut basins: Vec<Option<usize>> = vec![None; map.tiles.len()];
    let mut basin_index = 0;
    for start in 0..map.tiles.len() {
        if map.tiles[start] == 9 || basins[start].is_some() {
            continue;
        }

        let mut search = vec![start];
        basins[start] = Some(basin_index);
        while search.len() > 0 {
            let mut new_search = vec![];
            for pos in &search {
                for neighbor in map.neighbors(*pos) {
                    if basins[neighbor].is_some() || map.tiles[neighbor] == 9 {
                        continue;
                    }

                    basins[neighbor] = Some(basin_index);
                    new_search.push(neighbor);
                }
            }
            search = new_search;
        }

        basin_index += 1;
    }

    let mut counts = HashMap::new();
    for n in basins {
        if let Some(n) = n {
            *counts.entry(n).or_default() += 1;
        }
    }
    let mut sizes: Vec<usize> = counts.values().cloned().collect();
    sizes.sort();
    sizes.iter().skip(sizes.len() - 3).product()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
