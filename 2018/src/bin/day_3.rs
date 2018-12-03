use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Default)]
struct Pos {
    x: i32,
    y: i32,
}

fn main() {
    let file = File::open("input/day_3.txt").unwrap();
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .collect();

    let mut tiles: HashMap<Pos, Vec<i32>> = HashMap::new();
    for line in lines {
        let parts: Vec<&str> = line.split(" ").collect();
        let id = parts[0][1..].parse::<i32>().unwrap();
        let margins: Vec<i32> = parts[2][0..parts[2].len() - 1].split(",").map(|s| s.parse::<i32>().unwrap()).collect();
        let sizes: Vec<i32> = parts[3].split("x").map(|s| s.parse::<i32>().unwrap()).collect();

        for x in margins[0] .. margins[0] + sizes[0] {
            for y in margins[1] .. margins[1] + sizes[1] {
                tiles.entry(Pos { x, y }).or_default().push(id);
            }
        }
    }

    println!("Part 1: {}", tiles.values().filter(|v| v.len() > 1).count());

    let mut keys: Vec<i32> = tiles.values().flat_map(|v| v.iter()).cloned().collect();
    for x in tiles {
        if x.1.len() > 1 {
            keys.retain(|k| !x.1.contains(k));
        }
    }
    println!("Part 2: {}", keys[0]);
}
