use itertools::Itertools;
use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_17.txt");

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
struct Pos {
    pub x: i32,
    pub y: i32,
}

impl Pos {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    fn below(&self) -> Self {
        Self::new(self.x, self.y + 1)
    }
}

fn parse() -> HashSet<Pos> {
    let mut result = HashSet::new();
    for line in INPUT.lines() {
        let parts: Vec<_> = line.split(", ").flat_map(|s| s.split("=")).collect();
        let base = parts[1].parse().unwrap();
        let (from, to) = parts[3]
            .split("..")
            .map(|s| s.parse().unwrap())
            .collect_tuple()
            .unwrap();
        for n in from..=to {
            let pos = match parts[0] {
                "x" => Pos::new(base, n),
                "y" => Pos::new(n, base),
                _ => panic!(),
            };
            result.insert(pos);
        }
    }
    result
}

fn solve() -> (usize, usize) {
    let clay = parse();
    let (min_pos, max_pos) = clay
        .iter()
        .minmax_by_key(|pos| pos.y)
        .into_option()
        .unwrap();
    let mut filled = HashSet::new();
    let mut water_at_rest = HashSet::new();
    let mut falling = vec![(Pos::new(500, min_pos.y - 1), false)];
    while let Some((pos, is_rising)) = falling.pop() {
        if pos.y >= max_pos.y {
            continue;
        }

        let below = pos.below();
        if is_rising || clay.contains(&below) {
            let mut spilled = false;
            let mut new_water = vec![];
            for dir in &[-1, 1] {
                for n in 0.. {
                    let new_pos = Pos::new(pos.x + n * dir, pos.y);
                    if clay.contains(&new_pos) {
                        break;
                    }
                    filled.insert(new_pos);
                    new_water.push(new_pos);
                    let new_below = new_pos.below();
                    if !clay.contains(&new_below) && !filled.contains(&new_below) {
                        spilled = true;
                        if filled.contains(&Pos::new(new_below.x - dir, new_below.y)) {
                            // Avoid spilling further from a filled pool
                            filled.remove(&new_pos);
                            break;
                        }
                        filled.insert(new_below);
                        falling.push((new_below, false));
                        break;
                    }
                }
            }
            if !spilled {
                falling.push((Pos::new(pos.x, pos.y - 1), true));
                water_at_rest.extend(new_water);
            }
        } else {
            falling.push((below, false));
            filled.insert(below);
        }
    }
    (filled.len(), water_at_rest.len())
}

fn main() {
    let (part1, part2) = solve();
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
