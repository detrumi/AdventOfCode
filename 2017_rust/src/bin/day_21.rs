use bitvec::{prelude::BitVec, slice::BitSlice};
use itertools::Itertools;
use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_21.txt");

fn parse_pattern(s: &str) -> BitVec {
    s.chars().map(|c| c == '#').collect()
}

fn get_size(b: &BitSlice) -> usize {
    (b.len() as f32).sqrt() as usize
}

fn rotate(b: &BitSlice) -> BitVec {
    let size = get_size(b);
    let mut result = BitVec::new();
    for y in 0..size {
        for x in 0..size {
            result.push(b[size * (size - 1 - x) + y]);
        }
    }
    result
}

fn flip(b: &BitSlice) -> BitVec {
    let size = get_size(b);
    let mut result = BitVec::new();
    for y in 0..size {
        for x in 0..size {
            result.push(b[size * (size - 1 - y) + x])
        }
    }
    result
}

fn rules() -> HashMap<BitVec, BitVec> {
    let mut result = HashMap::new();
    for line in INPUT.trim().lines() {
        let (mut from, to) = line
            .replace("/", "")
            .splitn(2, " => ")
            .map(parse_pattern)
            .collect_tuple()
            .unwrap();
        for _ in 0..2 {
            for _ in 0..4 {
                result.insert(from.clone(), to.clone());
                from = rotate(&from);
            }
            from = flip(&from);
        }
    }
    result
}

fn divide(b: &BitSlice, cut_size: usize) -> Vec<BitVec> {
    let grid_rows = get_size(b);
    let tile_rows = grid_rows / cut_size;
    let mut result = vec![];
    for ty in 0..tile_rows {
        for tx in 0..tile_rows {
            let mut tile = BitVec::new();
            for y in 0..cut_size {
                for x in 0..cut_size {
                    let target_y = cut_size * ty + y;
                    let target_x = cut_size * tx + x;
                    tile.push(b[grid_rows * target_y + target_x]);
                }
            }
            result.push(tile);
        }
    }
    result
}

fn join(tiles: &[BitVec]) -> BitVec {
    let tile_size = get_size(&tiles[0]);
    let grid_rows = (tiles.len() as f32).sqrt() as usize;
    let grid_size = tile_size * grid_rows;
    let mut result = BitVec::new();
    for y in 0..grid_size {
        for x in 0..grid_size {
            let tile_y = y / tile_size;
            let tile_x = x / tile_size;
            let target_tile = &tiles[grid_rows * tile_y + tile_x];
            result.push(target_tile[tile_size * (y % tile_size) + (x % tile_size)]);
        }
    }
    result
}

fn enhance(b: &BitSlice, rules: &HashMap<BitVec, BitVec>) -> BitVec {
    let cut_size = if get_size(b) % 2 == 0 { 2 } else { 3 };
    let tiles = divide(b, cut_size);
    let enhanced: Vec<_> = tiles.iter().map(|t| rules[t].clone()).collect();
    join(&enhanced)
}

fn process(iterations: u32) -> usize {
    let rules = rules();
    let mut pattern = parse_pattern(".#...####");
    for _ in 0..iterations {
        pattern = enhance(&pattern, &rules);
    }
    pattern.count_ones()
}

fn main() {
    println!("Part 1: {}", process(5));
    println!("Part 2: {}", process(18));
}
