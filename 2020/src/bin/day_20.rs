use itertools::iproduct;
use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_20.txt");
const MONSTER: [&'static str; 3] = [
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   ",
];

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

fn parse_tile<'a>(lines: impl Iterator<Item = &'a str>) -> Tile {
    lines
        .map(|line| line.chars().map(|c| c == '#').collect())
        .collect()
}

fn parse() -> HashMap<Configuration, Tile> {
    let mut result = HashMap::new();
    for chunk in INPUT.split("\n\n") {
        let lines: Vec<_> = chunk.lines().collect();
        let id = lines[0]
            .trim_matches(|c: char| !c.is_digit(10))
            .parse()
            .unwrap();
        let mut tile = parse_tile(lines.into_iter().skip(1));

        for &flipped in &[false, true] {
            for rotation in 0..4 {
                result.insert(
                    Configuration {
                        id,
                        rotation,
                        flipped,
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
        let mut arrangement = vec![vec![]; size];
        arrangement[0].push(starting_conf);
        let mut x = 1;
        for y in 0..size {
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
                } else {
                    continue 'outer;
                }
            }
            x = 0;
        }
        return arrangement;
    }
    unreachable!()
}

fn part1(arrangement: &Vec<Vec<Configuration>>) -> usize {
    arrangement[0][0].id
        * arrangement[0][arrangement.len() - 1].id
        * arrangement[arrangement.len() - 1][0].id
        * arrangement[arrangement.len() - 1][arrangement.len() - 1].id
}

fn arrange(arrangement: &Vec<Vec<Configuration>>, tiles: &HashMap<Configuration, Tile>) -> Tile {
    arrangement
        .iter()
        .map(|tile_row| tile_row.iter().map(|c| tiles[c].clone()))
        .flat_map(|row_tiles| {
            (1..tiles.values().next().unwrap().len() - 1).map(move |row| {
                row_tiles
                    .clone()
                    .flat_map(|tile| tile[row][1..tile[row].len() - 1].to_vec())
                    .collect()
            })
        })
        .collect()
}

fn part2(tile: Tile) -> usize {
    let mut monster = parse_tile(MONSTER.iter().cloned());
    let mut monster_parts = vec![vec![false; tile[0].len()]; tile.len()];
    for _f in 0..2 {
        for _r in 0..4 {
            for y in 0..tile.len() - monster.len() {
                for x in 0..tile[0].len() - monster[0].len() {
                    let offsets = iproduct!(0..monster.len(), 0..monster[0].len())
                        .filter(|&(dy, dx)| monster[dy][dx]);
                    if offsets.clone().all(|(dy, dx)| tile[y + dy][x + dx]) {
                        offsets.for_each(|(dy, dx)| monster_parts[y + dy][x + dx] = true);
                    }
                }
            }

            monster = rotate(monster);
            monster_parts = rotate(monster_parts);
        }
        monster = flip(monster);
        monster_parts = flip(monster_parts);
    }

    let total = tile
        .iter()
        .flat_map(|line| line.iter().filter(|b| **b))
        .count();
    let parts = monster_parts
        .iter()
        .flat_map(|line| line.iter().filter(|b| **b))
        .count();
    total - parts
}

fn main() {
    let tiles = parse();
    let arrangement = find_arrangement(&tiles);
    println!("Part 1: {}", part1(&arrangement));
    println!("Part 2: {}", part2(arrange(&arrangement, &tiles)));
}
