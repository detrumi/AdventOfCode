use std::collections::BinaryHeap;

const INPUT: &str = include_str!("../../input/day_15.txt");

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
struct Pos {
    pub x: isize,
    pub y: isize,
}

impl Pos {
    pub fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }
}

#[derive(Eq, Default, Debug)]
struct Path {
    pub risk: usize,
    pub pos: Pos,
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos && self.risk == other.risk
    }
}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.risk.partial_cmp(&self.risk)
    }
}

impl Ord for Path {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.risk.cmp(&self.risk)
    }
}

fn parse() -> Vec<Vec<usize>> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect()
}

struct Map {
    levels: Vec<Vec<usize>>,
    height: isize,
    width: isize,
}

impl Map {
    pub fn new() -> Self {
        let levels = parse();
        Self {
            height: levels.len() as isize,
            width: levels[0].len() as isize,
            levels,
        }
    }

    pub fn upscaled() -> Self {
        let levels = parse();

        let mut new_levels: Vec<Vec<usize>> = (0..levels.len())
            .map(|y| {
                (0..5)
                    .flat_map(|offset| levels[y].iter().map(move |n| (n - 1 + offset) % 9 + 1))
                    .collect()
            })
            .collect();

        for offset in 1..5 {
            (0..levels.len()).for_each(|y| {
                new_levels.push(
                    new_levels[y]
                        .iter()
                        .map(|n| (n - 1 + offset) % 9 + 1)
                        .collect(),
                )
            });
        }

        Self {
            height: new_levels.len() as isize,
            width: new_levels[0].len() as isize,
            levels: new_levels,
        }
    }

    pub fn neighbors(&self, pos: Pos) -> Vec<Pos> {
        let mut neighbors = vec![];
        for (dx, dy) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
            if pos.y + dy >= 0
                && pos.y + dy < self.height
                && pos.x + dx >= 0
                && pos.x + dx < self.width
            {
                neighbors.push(Pos::new(pos.x + dx, pos.y + dy));
            }
        }
        neighbors
    }
}

fn solve(map: Map) -> usize {
    let mut best_risks = vec![vec![None; map.width as usize]; map.height as usize];
    let mut paths: BinaryHeap<Path> = BinaryHeap::new();
    paths.push(Path::default());
    while let Some(Path { risk, pos }) = paths.pop() {
        for neighbor in map.neighbors(pos) {
            if best_risks[neighbor.y as usize][neighbor.x as usize].is_none() {
                let new_risk = risk + map.levels[neighbor.y as usize][neighbor.x as usize];
                best_risks[neighbor.y as usize][neighbor.x as usize] = Some(new_risk);
                paths.push(Path {
                    risk: new_risk,
                    pos: neighbor,
                });
            }
        }
    }
    best_risks[map.height as usize - 1][map.width as usize - 1].unwrap()
}

fn main() {
    println!("Part 1: {}", solve(Map::new()));
    println!("Part 2: {}", solve(Map::upscaled()));
}
