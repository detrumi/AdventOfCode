use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_9.txt");

fn part1() -> u32 {
    let lines: Vec<_> = INPUT.trim().lines().collect();
    let width = lines[0].len();
    let height = lines.len();
    let input: Vec<u32> = lines
        .iter()
        .flat_map(|line| line.chars().map(|c| char::to_digit(c, 10).unwrap()))
        .collect();
    let mut result = 0;
    for y in 0..height {
        for x in 0..width {
            let pos = (width * y + x) as isize;
            let neighbors = vec![pos - width as isize, pos - 1, pos + 1, pos + width as isize];
            if neighbors.iter().all(|n| {
                *n < 0
                    || (*n as usize) >= width * height
                    || input[width * y + x] < input[*n as usize]
            }) {
                result += 1 + input[pos as usize];
            }
        }
    }
    result
}

fn part2() -> usize {
    let lines: Vec<_> = INPUT.trim().lines().collect();
    let width = lines[0].len();
    let height = lines.len();
    let input: Vec<u32> = lines
        .iter()
        .flat_map(|line| line.chars().map(|c| char::to_digit(c, 10).unwrap()))
        .collect();

    let mut basins: Vec<Option<usize>> = vec![None; input.len()];
    let mut basin_index = 0;
    for start in 0..input.len() as isize {
        if input[start as usize] == 9 || basins[start as usize].is_some() {
            continue;
        }

        let mut search = vec![start];
        basins[start as usize] = Some(basin_index);
        while search.len() > 0 {
            let mut new_search = vec![];
            for pos in &search {
                let x = pos % width as isize;
                let y = pos / width as isize;
                for (dx, dy) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
                    if x + dx < 0
                        || y + dy < 0
                        || x + dx >= width as isize
                        || y + dy >= height as isize
                    {
                        continue;
                    }
                    let neighbor = width as isize * (y + dy) + x + dx;
                    if basins[neighbor as usize].is_some() || input[neighbor as usize] == 9 {
                        continue;
                    }

                    basins[neighbor as usize] = Some(basin_index);
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
