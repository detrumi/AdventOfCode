#![feature(destructuring_assignment)]

use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_20.txt");
type Tile = Vec<Vec<bool>>;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
struct Configuration {
    pub id: usize,
    pub rotation: usize,
    pub flipped: bool,
}

fn flip(tile: Tile) -> Tile {
    tile.into_iter().rev().collect()
}

fn rotate(tile: Tile) -> Tile {
    let mut result = vec![vec![false; tile.len()]; tile[0].len()];
    for y in 0..tile[0].len() {
        for x in 0..tile.len() {
            result[y][tile.len() - x - 1] = tile[x][y];
        }
    }
    result
}

fn parse() -> HashMap<Configuration, Tile> {
    let mut result = HashMap::new();
    for chunk in INPUT.split("\n\n") {
        let lines: Vec<_> = chunk.lines().collect();
        let id = lines[0]
            .trim_start_matches("Tile ")
            .trim_end_matches(':')
            .parse()
            .unwrap();
        let mut tile: Tile = lines
            .iter()
            .skip(1)
            .map(|line| line.chars().map(|c| c == '#').collect())
            .collect();

        for f in 0..2 {
            for r in 0..4 {
                result.insert(
                    Configuration {
                        id,
                        rotation: r,
                        flipped: f != 0,
                    },
                    tile.clone(),
                );
                tile = rotate(tile);
            }
            tile = flip(tile);
        }
    }
    result
}

fn tiles_match(tile: &Tile, neighbor: &Tile, side: usize) -> bool {
    let end = tile.len() - 1;
    match side {
        0 => tile[end] == neighbor[0],
        1 => itertools::equal(
            tile.iter().map(|row| row[end]),
            neighbor.iter().map(|row| row[0]),
        ),
        2 => tile[0] == neighbor[end],
        3 => itertools::equal(
            tile.iter().map(|row| row[0]),
            neighbor.iter().map(|row| row[end]),
        ),
        _ => unreachable!(),
    }
}

fn find_arrangement(tiles: &HashMap<Configuration, Tile>) -> Vec<Vec<Configuration>> {
    let size = (tiles.len() as f32 / 8.0).sqrt() as usize;
    'outer: for (&starting_conf, _starting_tile) in tiles {
        let mut arrangement = vec![vec![starting_conf]];

        let (mut y, mut x) = (0, 1);
        while y < size {
            while x < size {
                let (side, last_conf) = match x {
                    0 => (0, arrangement[y - 1][0]),
                    _ => (1, arrangement[y][x - 1]),
                };

                if let Some((new_conf, _tile)) = tiles
                    .iter()
                    .filter(|(c, _)| c.id != last_conf.id)
                    .find(|(_c, neighbor)| tiles_match(&tiles[&last_conf], neighbor, side))
                {
                    arrangement[y].push(*new_conf);
                    x += 1;
                    if x == size {
                        x = 0;
                        y += 1;
                        arrangement.push(vec![]);
                        break;
                    }
                } else {
                    continue 'outer;
                }
            }
        }
        arrangement.pop();
        return arrangement;
    }
    panic!()
}

fn part1(arrangement: &Vec<Vec<Configuration>>) -> usize {
    arrangement[0][0].id
        * arrangement[0][arrangement.len() - 1].id
        * arrangement[arrangement.len() - 1][0].id
        * arrangement[arrangement.len() - 1][arrangement.len() - 1].id
}

fn arrange(arrangement: &Vec<Vec<Configuration>>, tiles: &HashMap<Configuration, Tile>) -> Tile {
    let len = tiles.values().next().unwrap().len();
    let mut big_tile = vec![];
    for tile_row in arrangement {
        let row_tiles: Vec<Tile> = tile_row.iter().map(|c| tiles[c].clone()).collect();
        for row in 1..len - 1 {
            big_tile.push(
                row_tiles
                    .iter()
                    .flat_map(|tile| tile[row][1..tile[row].len() - 1].to_vec())
                    .collect(),
            );
        }
    }
    big_tile
}

fn part2(big_tile: Tile) -> usize {
    let mut monster: Tile = [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   ",
    ]
    .iter()
    .map(|line| line.chars().map(|c| c == '#').collect())
    .collect();

    let mut monster_parts = vec![vec![false; big_tile[0].len()]; big_tile.len()];
    for _f in 0..2 {
        for _r in 0..4 {
            for y in 0..big_tile.len() - monster.len() {
                'outer: for x in 0..big_tile[0].len() - monster[0].len() {
                    for dy in 0..monster.len() {
                        for dx in 0..monster[0].len() {
                            if monster[dy][dx] && !big_tile[y + dy][x + dx] {
                                continue 'outer;
                            }
                        }
                    }
                    for dy in 0..monster.len() {
                        for dx in 0..monster[0].len() {
                            if monster[dy][dx] {
                                monster_parts[y + dy][x + dx] = true;
                            }
                        }
                    }
                }
            }

            monster = rotate(monster);
            monster_parts = rotate(monster_parts);
        }
        monster = flip(monster);
        monster_parts = flip(monster_parts);
    }

    let total: usize = big_tile
        .iter()
        .map(|line| line.iter().filter(|b| **b).count())
        .sum();
    let parts: usize = monster_parts
        .iter()
        .map(|line| line.iter().filter(|b| **b).count())
        .sum();
    total - parts
}

fn main() {
    let tiles = parse();
    let arrangement = find_arrangement(&tiles);
    println!("Part 1: {}", part1(&arrangement));
    println!("Part 2: {}", part2(arrange(&arrangement, &tiles)));
}
