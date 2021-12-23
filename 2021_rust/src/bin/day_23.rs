use std::{
    collections::{BinaryHeap, HashMap},
    fmt::Display,
};

const INPUT: &str = include_str!("../../input/day_23.txt");

#[derive(Clone, Copy, Hash, Debug)]
struct Pos {
    pub x: usize,
    pub y: usize,
}

impl Pos {
    pub fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum Tile {
    Empty,
    Wall,
    Amphipod(char),
}

fn energy_for_char(c: char) -> usize {
    match c {
        'A' => 1,
        'B' => 10,
        'C' => 100,
        'D' => 1000,
        _ => panic!(),
    }
}

impl From<char> for Tile {
    fn from(c: char) -> Self {
        match c {
            '.' => Self::Empty,
            '#' | ' ' => Self::Wall,
            _ => Self::Amphipod(c),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct Map {
    tiles: Vec<Vec<Tile>>,
}

impl Map {
    fn new(lines: &[&'_ str]) -> Self {
        let tiles = lines
            .iter()
            .map(|line| line.chars().map(|c| Tile::from(c)).collect())
            .collect();
        Self { tiles }
    }

    fn room_length(&self) -> usize {
        self.tiles.len() - 3
    }

    fn score(&self) -> usize {
        let mut result = 0;
        for x in [3, 5, 7, 9] {
            let expected = expected_for_room(x);
            for y in (2..=self.room_length() + 1).rev() {
                match self.tiles[y][x] {
                    Tile::Amphipod(c) if c == expected => result += 1,
                    _ => break,
                }
            }
        }
        result
    }
}

impl Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in 0..self.tiles.len() {
            for x in 0..self.tiles[y].len() {
                write!(
                    f,
                    "{}",
                    match self.tiles[y][x] {
                        Tile::Empty => '.',
                        Tile::Wall => '#',
                        Tile::Amphipod(c) => c,
                    }
                )?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Eq, Hash, Debug)]
struct State {
    map: Map,
    score: usize,
    energy: usize,
}

impl State {
    fn new(map: Map, energy: usize) -> Self {
        Self {
            score: map.score(),
            map,
            energy,
        }
    }

    fn score(&self) -> isize {
        100 * self.map.score() as isize - self.energy as isize
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.score == other.score && self.energy == other.energy
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.score().partial_cmp(&other.score())
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.score().cmp(&other.score())
    }
}

fn expected_for_room(x: usize) -> char {
    match x {
        3 => 'A',
        5 => 'B',
        7 => 'C',
        9 => 'D',
        _ => panic!(),
    }
}

fn movable_amphipods(map: &Map) -> Vec<(Pos, Pos)> {
    let mut result = vec![];
    for y in 0..map.tiles.len() {
        for x in 0..map.tiles[y].len() {
            if let Tile::Amphipod(c) = map.tiles[y][x] {
                if y == 1 {
                    // In hallway
                    for sign in [-1, 1] {
                        for dx in 1.. {
                            let target_x = (x as isize + sign * dx) as usize;
                            if map.tiles[1][target_x] != Tile::Empty {
                                break;
                            }
                            if [3, 5, 7, 9].contains(&target_x) {
                                // Only enter if room contains no other kinds
                                if c != expected_for_room(target_x)
                                    || (2..=map.room_length() + 1).any(|ty| {
                                        map.tiles[ty][target_x] != Tile::Empty
                                            && map.tiles[ty][target_x] != Tile::Amphipod(c)
                                    })
                                {
                                    continue;
                                }
                                for target_y in 2..=map.room_length() + 1 {
                                    if map.tiles[target_y][target_x] != Tile::Empty {
                                        break;
                                    }
                                    // Move only to last spot in room
                                    if map.tiles[target_y + 1][target_x] != Tile::Empty {
                                        result.push((Pos::new(x, y), Pos::new(target_x, target_y)));
                                        break;
                                    }
                                }
                            }
                        }
                    }
                } else if [3, 5, 7, 9].contains(&x) {
                    // In room
                    if (1..y)
                        .into_iter()
                        .any(|target_y| map.tiles[target_y][x] != Tile::Empty)
                    {
                        continue;
                    }
                    for sign in [-1, 1] {
                        for dist in 1.. {
                            let target_x = (x as isize + dist * sign) as usize;
                            if map.tiles[1][target_x] != Tile::Empty {
                                break;
                            }
                            if ![3, 5, 7, 9].contains(&target_x) {
                                result.push((Pos::new(x, y), Pos::new(target_x, 1)));
                            }
                        }
                    }
                }
            }
        }
    }
    result
}

fn solve(starting_map: Map) -> usize {
    let mut heap = BinaryHeap::new();
    let mut visited_maps: HashMap<Map, usize> = HashMap::new();
    let mut best_energy = usize::MAX;
    heap.push(State::new(starting_map, 0));
    while let Some(State { map, energy, .. }) = heap.pop() {
        for (from, to) in movable_amphipods(&map) {
            if let Tile::Amphipod(c) = map.tiles[from.y][from.x] {
                let mut new_map = map.clone();
                new_map.tiles[to.y][to.x] = new_map.tiles[from.y][from.x];
                new_map.tiles[from.y][from.x] = Tile::Empty;
                let mut cost = (to.x as isize - from.x as isize).abs();
                cost += (to.y as isize - from.y as isize).abs();
                cost *= energy_for_char(c) as isize;
                let new_energy = energy + cost as usize;
                if new_energy > best_energy {
                    continue;
                }

                if let Some(old_energy) = visited_maps.get(&new_map) {
                    if *old_energy <= new_energy {
                        continue;
                    }
                }
                visited_maps.insert(new_map.clone(), new_energy);

                let mut is_end_position = true;
                'outer: for dx in 0..4 {
                    for y in 2..=map.room_length() + 1 {
                        let expected = ('A' as u8 + dx as u8) as char;
                        if new_map.tiles[y][3 + 2 * dx] != Tile::Amphipod(expected) {
                            is_end_position = false;
                            break 'outer;
                        }
                    }
                }
                if is_end_position {
                    best_energy = best_energy.min(new_energy);
                } else {
                    heap.push(State::new(new_map, new_energy as usize));
                }
            }
        }
    }
    best_energy
}

fn main() {
    let mut lines: Vec<_> = INPUT.trim().lines().collect();
    println!("Part 1: {}", solve(Map::new(&lines)));

    lines.insert(3, "  #D#C#B#A#");
    lines.insert(3, "  #D#B#A#C#");
    println!("Part 2: {}", solve(Map::new(&lines)));
}
