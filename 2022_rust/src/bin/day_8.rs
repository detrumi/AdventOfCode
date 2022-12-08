use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_8.txt");

fn parse() -> Vec<Vec<u32>> {
    INPUT
        .trim()
        .lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

fn part1() -> usize {
    let grid = parse();
    let mut visible = HashSet::new();
    for y in 0..grid.len() {
        let mut last = 0;
        let mut first = true;
        for x in 0..grid[0].len() {
            if first || grid[y][x] > last {
                visible.insert((x, y));
                last = grid[y][x];
            }
            first = false;
        }

        let mut last = 0;
        let mut first = true;
        for x in (0..grid[0].len()).rev() {
            if first || grid[y][x] > last {
                visible.insert((x, y));
                last = grid[y][x];
            }
            first = false;
        }
    }
    for x in 0..grid.len() {
        let mut last = 0;
        let mut first = true;
        for y in 0..grid[0].len() {
            if first || grid[y][x] > last {
                visible.insert((x, y));
                last = grid[y][x];
            }
            first = false;
        }

        let mut last = 0;
        let mut first = true;
        for y in (0..grid[0].len()).rev() {
            if first || grid[y][x] > last {
                visible.insert((x, y));
                last = grid[y][x];
            }
            first = false;
        }
    }
    visible.len()
}

fn part2() -> usize {
    let grid = parse();
    let mut result = 0;
    for y in 0..grid.len() {
        for x in 0..grid.len() {
            let start = grid[y][x];
            let mut score = 1;
            for (dy, dx) in &[(1, 0), (0, 1), (-1, 0), (0, -1)] {
                let mut distance = 0;
                for d in 1.. {
                    let x2 = x as i32 + d as i32 * dx;
                    let y2 = y as i32 + d as i32 * dy;
                    if x2 < 0 || y2 < 0 || x2 >= grid.len() as i32 || y2 >= grid.len() as i32 {
                        break;
                    }
                    distance += 1;
                    if grid[y2 as usize][x2 as usize] >= start {
                        break;
                    }
                }
                score *= distance;
            }
            result = result.max(score);
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
