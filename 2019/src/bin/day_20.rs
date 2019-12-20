use itertools::sorted;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Default)]
struct Pos {
    x: usize,
    y: usize,
}

impl Pos {
    pub fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    pub fn neighbors(self, size: Pos) -> Vec<Self> {
        let mut result = vec![];
        if self.x > 0 {
            result.push(Pos::new(self.x - 1, self.y))
        }
        if self.x + 1 < size.x {
            result.push(Pos::new(self.x + 1, self.y))
        }
        if self.y > 0 {
            result.push(Pos::new(self.x, self.y - 1))
        }
        if self.y + 1 < size.y {
            result.push(Pos::new(self.x, self.y + 1))
        }
        result
    }

    pub fn is_outer(self, size: Pos) -> bool {
        self.x <= 4 || self.y <= 4 || self.x >= size.x - 4 || self.y >= size.y - 4
    }
}

#[derive(Clone, Debug)]
enum Tile {
    Wall,
    Passage,
    Portal(char),
}

fn main() {
    let file = File::open("input/day_20.txt").unwrap();
    let lines: Vec<_> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap().chars().collect::<Vec<_>>())
        .collect();

    let size = Pos::new(lines.iter().map(|l| l.len()).max().unwrap(), lines.len());

    let mut tiles = vec![vec![Tile::Wall; size.x]; size.y];
    let mut letter_locations: HashMap<Pos, char> = HashMap::new();
    for y in 0..lines.len() {
        for x in 0..lines[y].len() {
            tiles[y][x] = match lines[y][x] {
                '#' | ' ' => Tile::Wall,
                '.' => Tile::Passage,
                c => {
                    letter_locations.insert(Pos::new(x, y), c);
                    Tile::Portal(c)
                }
            }
        }
    }

    let mut start = Pos::default();
    let portals: Vec<(Pos, String)> = letter_locations
        .iter()
        .filter_map(|(pos, c1)| {
            let mut c2 = '?';
            let mut passage = None;
            for neighbor in pos.neighbors(size) {
                match tiles[neighbor.y][neighbor.x] {
                    Tile::Portal(c) => c2 = c,
                    Tile::Passage => passage = Some(neighbor),
                    _ => (),
                }
            }
            passage.map(|passage| {
                if *c1 == 'A' && c2 == 'A' {
                    start = passage;
                }
                (passage, sorted(vec![*c1, c2].iter()).collect::<String>())
            })
        })
        .collect();

    println!("Part 1 = {}", part1(&tiles, &portals, start, size));
    println!("Part 2 = {}", part2(&tiles, &portals, start, size));
}

fn part1(tiles: &[Vec<Tile>], portals: &[(Pos, String)], start: Pos, size: Pos) -> i32 {
    let mut visited: HashSet<Pos> = HashSet::new();
    visited.insert(start);
    let mut queue: VecDeque<(i32, Pos)> = VecDeque::new();
    queue.push_back((0, start));
    while let Some((dist, pos)) = queue.pop_front() {
        for mut neighbor in pos.neighbors(size) {
            match tiles[neighbor.y][neighbor.x] {
                Tile::Wall => continue,
                Tile::Passage => (),
                Tile::Portal(_) => {
                    let (_, portal) = portals.iter().find(|(p, _)| *p == pos).unwrap();
                    if portal == "AA" {
                        continue;
                    } else if portal == "ZZ" {
                        return dist;
                    }
                    let target_portal: String = sorted(portal.chars()).collect();
                    let (target, _) = portals
                        .iter()
                        .find(|(p, t)| *t == target_portal && *p != pos)
                        .unwrap();
                    neighbor = *target;
                }
            }
            if visited.insert(neighbor) {
                queue.push_back((dist + 1, neighbor));
            }
        }
    }
    panic!()
}

fn part2(tiles: &[Vec<Tile>], portals: &[(Pos, String)], start: Pos, size: Pos) -> i32 {
    let mut visited: HashSet<(Pos, i32)> = HashSet::new();
    visited.insert((start, 0));
    let mut queue: VecDeque<(i32, Pos, i32)> = VecDeque::new();
    queue.push_back((0, start, 0));
    while let Some((dist, pos, depth)) = queue.pop_front() {
        for mut neighbor in pos.neighbors(size) {
            let mut new_depth = depth;
            match tiles[neighbor.y][neighbor.x] {
                Tile::Wall => continue,
                Tile::Passage => (),
                Tile::Portal(_) => {
                    let (_, portal) = portals.iter().find(|(p, _)| *p == pos).unwrap();
                    if portal == "ZZ" {
                        if depth > 0 {
                            continue;
                        }
                        return dist;
                    } else if portal == "AA" || (depth == 0 && pos.is_outer(size)) {
                        continue;
                    }
                    let target_portal: String = sorted(portal.chars()).collect();
                    let (target, _) = portals
                        .iter()
                        .find(|(p, t)| *t == target_portal && *p != pos)
                        .unwrap();
                    new_depth += if pos.is_outer(size) { -1 } else { 1 };
                    neighbor = *target;
                }
            }
            if visited.insert((neighbor, new_depth)) {
                queue.push_back((dist + 1, neighbor, new_depth));
            }
        }
    }
    panic!()
}
