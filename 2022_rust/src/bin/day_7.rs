use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_7.txt");

fn calc_size(outer_dir: String, dir_size: usize, files: &HashMap<String, usize>) -> Option<usize> {
    let mut size = dir_size;
    if size > 100000 {
        return None;
    }
    for (dir, dir_size) in files {
        if !dir.starts_with(&format!("{outer_dir}/")) {
            continue;
        }
        size += calc_size(dir.clone(), *dir_size, files)?;
        if size > 100000 {
            return None;
        }
    }
    Some(size)
}

fn part1() -> usize {
    let mut files: HashMap<String, HashMap<String, usize>> = HashMap::new();
    let mut dir = PathBuf::new();
    let mut lines = INPUT.trim().lines().peekable();
    while let Some(line) = lines.next() {
        let parts = line.split_ascii_whitespace().collect_vec();
        if parts[0] == "$" {
            match parts[1] {
                "cd" if parts[2] == "/" => dir = PathBuf::new(),
                "cd" if parts[2] == ".." => {
                    dir.pop();
                }
                "cd" => dir.push(parts[2]),
                "ls" => {
                    while let Some(line) = lines.peek() {
                        if line.starts_with("$") {
                            break;
                        }
                        let parts = line.split_ascii_whitespace().collect_vec();
                        if parts[0] == "dir" {
                        } else {
                            let size: usize = parts[0].parse().unwrap();
                            let current_dir =
                                files.entry(dir.to_str().unwrap().to_string()).or_default();
                            *current_dir.entry(parts[1].to_string()).or_default() += size;
                        }
                        lines.next();
                    }
                }
                _ => (),
            }
        }
    }
    let files: HashMap<String, usize> = files
        .into_iter()
        .map(|(dir, files)| {
            let size = files.values().sum();
            (dir, size)
        })
        .collect();

    let mut result = 0;
    let mut all_dirs = HashSet::new();
    for (dir, _) in &files {
        let mut dir: String = dir.clone();
        all_dirs.insert(dir.clone());
        while dir.contains("/") {
            while let Some(c) = dir.pop() {
                if c == '/' {
                    break;
                }
            }
            all_dirs.insert(dir.clone());
        }
    }
    for dir in all_dirs {
        let dir_size = files.get(&dir).unwrap_or(&0);
        let size = calc_size(dir.clone(), *dir_size, &files);
        if let Some(size) = size {
            result += size;
        }
    }

    result
}

fn part2() -> usize {
    let mut files: HashMap<String, HashMap<String, usize>> = HashMap::new();
    let mut dir = PathBuf::new();
    let mut lines = INPUT.trim().lines().peekable();
    while let Some(line) = lines.next() {
        let parts = line.split_ascii_whitespace().collect_vec();
        if parts[0] == "$" {
            match parts[1] {
                "cd" if parts[2] == "/" => dir = PathBuf::new(),
                "cd" if parts[2] == ".." => {
                    dir.pop();
                }
                "cd" => dir.push(parts[2]),
                "ls" => {
                    while let Some(line) = lines.peek() {
                        if line.starts_with("$") {
                            break;
                        }
                        let parts = line.split_ascii_whitespace().collect_vec();
                        if parts[0] == "dir" {
                        } else {
                            let size: usize = parts[0].parse().unwrap();
                            let current_dir =
                                files.entry(dir.to_str().unwrap().to_string()).or_default();
                            *current_dir.entry(parts[1].to_string()).or_default() += size;
                        }
                        lines.next();
                    }
                }
                _ => (),
            }
        }
    }
    let files: HashMap<String, usize> = files
        .into_iter()
        .map(|(dir, files)| {
            let size = files.values().sum();
            (dir, size)
        })
        .collect();

    let mut all_dirs = HashSet::new();
    for (dir, _) in &files {
        let mut dir: String = dir.clone();
        all_dirs.insert(dir.clone());
        while dir.contains("/") {
            while let Some(c) = dir.pop() {
                if c == '/' {
                    break;
                }
            }
            all_dirs.insert(dir.clone());
        }
    }
    let available = 70000000;
    let total_size: usize = files.values().sum();
    let unused = available - total_size;
    let remaining = 30000000 - unused;

    let mut result = usize::MAX;

    for dir in all_dirs {
        if dir.is_empty() {
            continue;
        }
        let size: usize = files
            .iter()
            .filter(|(d, _)| d.starts_with(&dir))
            .map(|(_, n)| n)
            .sum();
        if size >= remaining && size < result {
            result = size;
        }
    }

    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
