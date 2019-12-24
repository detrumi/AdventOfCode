use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_24.txt").unwrap();
    let lines: Vec<_> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap().chars().map(|c| c == '#').collect::<Vec<bool>>())
        .collect();

    let mut tiles = vec![vec![false; 5 + 2]];
    for line in &lines {
        let mut line2 = vec![false];
        for tile in line {
            line2.push(*tile);
        }
        line2.push(false);
        tiles.push(line2);
    }
    tiles.push(vec![false; 5 + 2]);

    println!("Part 1 = {}", part1(tiles));
    println!("Part 2 = {}", part2(lines));
}

fn part1(mut tiles: Vec<Vec<bool>>) -> usize {
    let mut found: HashSet<Vec<Vec<bool>>> = HashSet::new();
    while found.insert(tiles.clone()) {
        let mut new_tiles = tiles.clone();
        for y in 1..=5 {
            for x in 1..=5 {
                let mut count = 0;
                count += tiles[y - 1][x] as usize;
                count += tiles[y + 1][x] as usize;
                count += tiles[y][x - 1] as usize;
                count += tiles[y][x + 1] as usize;

                new_tiles[y][x] = if tiles[y][x] {
                    count == 1
                } else {
                    count == 1 || count == 2
                };
            }
        }
        tiles = new_tiles;
    }
    let mut rating = 0;
    let mut worth = 1;
    for y in 1..=5 {
        for x in 1..=5 {
            if tiles[y][x] {
                rating += worth;
            }
            worth *= 2;
        }
    }
    rating
}

fn part2(tiles: Vec<Vec<bool>>) -> usize {
    let mut levels: VecDeque<Vec<Vec<bool>>> = VecDeque::new();
    levels.push_back(tiles);
    for _ in 0..200 {
        let left_outer = (0..5).any(|y| levels[0][y][0]);
        let right_outer = (0..5).any(|y| levels[0][y][5 - 1]);
        let top_outer = (0..5).any(|x| levels[0][0][x]);
        let bottom_outer = (0..5).any(|x| levels[0][5 - 1][x]);
        if left_outer || right_outer || top_outer || bottom_outer {
            levels.push_front(vec![vec![false; 5]; 5]);
        }

        let left_inner = levels[levels.len() - 1][2][1];
        let right_inner = levels[levels.len() - 1][2][3];
        let top_inner = levels[levels.len() - 1][1][2];
        let bottom_inner = levels[levels.len() - 1][3][2];
        if left_inner || right_inner || top_inner || bottom_inner {
            levels.push_back(vec![vec![false; 5]; 5]);
        }

        let mut new_levels = levels.clone();
        for level in 0..levels.len() {
            for y in 0..5 {
                for x in 0..5 {
                    if x == 2 && y == 2 {
                        continue;
                    }
                    let mut count = 0;

                    // Left
                    if x == 0 {
                        if level != 0 {
                            count += levels[level - 1][2][1] as usize;
                        }
                    } else if x == 3 && y == 2 {
                        if level != levels.len() - 1 {
                            for y in 0..5 {
                                count += levels[level + 1][y][4] as usize;
                            }
                        }
                    } else {
                        count += levels[level][y][x - 1] as usize;
                    }

                    // Right
                    if x == 4 {
                        if level != 0 {
                            count += levels[level - 1][2][3] as usize;
                        }
                    } else if x == 1 && y == 2 {
                        if level != levels.len() - 1 {
                            for y in 0..5 {
                                count += levels[level + 1][y][0] as usize;
                            }
                        }
                    } else {
                        count += levels[level][y][x + 1] as usize;
                    }

                    // Top
                    if y == 0 {
                        if level != 0 {
                            count += levels[level - 1][1][2] as usize;
                        }
                    } else if y == 3 && x == 2 {
                        if level != levels.len() - 1 {
                            for x in 0..5 {
                                count += levels[level + 1][4][x] as usize;
                            }
                        }
                    } else {
                        count += levels[level][y - 1][x] as usize;
                    }

                    // Bottom
                    if y == 4 {
                        if level != 0 {
                            count += levels[level - 1][3][2] as usize;
                        }
                    } else if y == 1 && x == 2 {
                        if level != levels.len() - 1 {
                            for x in 0..5 {
                                count += levels[level + 1][0][x] as usize;
                            }
                        }
                    } else {
                        count += levels[level][y + 1][x] as usize;
                    }

                    new_levels[level][y][x] = count == 1 || (!levels[level][y][x] && count == 2);
                }
            }
        }
        levels = new_levels;
    }
    let mut bugs = 0;
    for level in &levels {
        for y in 0..5 {
            for x in 0..5 {
                if level[y][x] {
                    bugs += 1;
                }
            }
        }
    }
    bugs
}
