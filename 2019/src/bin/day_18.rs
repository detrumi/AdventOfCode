use bit_vec::BitVec;
use std::char;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{self, BufRead};

#[derive(Copy, Clone, PartialEq, Eq, Default)]
struct Pos {
    x: usize,
    y: usize,
}

impl Hash for Pos {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (100 * self.y + self.x).hash(state);
    }
}

impl Pos {
    pub fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    pub fn go_dir(self, dir: Dir) -> Self {
        match dir {
            Dir::North => Pos::new(self.x, self.y - 1),
            Dir::East => Pos::new(self.x + 1, self.y),
            Dir::South => Pos::new(self.x, self.y + 1),
            Dir::West => Pos::new(self.x - 1, self.y),
        }
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Dir {
    North,
    East,
    South,
    West,
}

#[derive(Clone, Copy)]
enum Tile {
    Wall,
    Start,
    Open,
    Door(u32),
    Key(u32),
}

impl Tile {
    pub fn new(c: char) -> Self {
        match c {
            '#' => Tile::Wall,
            '@' => Tile::Start,
            '.' => Tile::Open,
            c if c.is_uppercase() => Tile::Door(c as u32 - 'A' as u32),
            c => Tile::Key(c as u32 - 'a' as u32),
        }
    }

    pub fn to_key(self) -> Option<u32> {
        match self {
            Tile::Key(key) | Tile::Door(key) => Some(key),
            _ => None,
        }
    }
}

impl fmt::Debug for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = match self {
            Tile::Wall => '#',
            Tile::Start => '@',
            Tile::Open => '.',
            Tile::Door(key) => char::from_u32('A' as u32 + *key).unwrap(),
            Tile::Key(key) => char::from_u32('a' as u32 + key).unwrap(),
        };
        write!(f, "{}", c)
    }
}

fn main() {
    let file = File::open("input/day_18.txt").unwrap();
    let mut start = Pos::default();
    let mut num_keys = 0;
    let mut tiles: Vec<_> = io::BufReader::new(file)
        .lines()
        .enumerate()
        .map(|(y, l)| {
            l.unwrap()
                .chars()
                .enumerate()
                .map(|(x, c)| {
                    let tile = Tile::new(c);
                    match tile {
                        Tile::Start => start = Pos::new(x, y),
                        Tile::Key(_) => num_keys += 1,
                        _ => (),
                    }
                    tile
                })
                .collect::<Vec<_>>()
        })
        .collect();

    // Part 1:
    // calculate(&tiles, vec![start], num_keys);

    let start_positions = vec![
        Pos::new(start.x + 1, start.y + 1),
        Pos::new(start.x + 1, start.y - 1),
        Pos::new(start.x - 1, start.y + 1),
        Pos::new(start.x - 1, start.y - 1),
    ];
    for pos in &start_positions {
        tiles[pos.y][pos.x] = Tile::Start;
    }
    tiles[start.y][start.x] = Tile::Wall;
    tiles[start.y - 1][start.x] = Tile::Wall;
    tiles[start.y + 1][start.x] = Tile::Wall;
    tiles[start.y][start.x - 1] = Tile::Wall;
    tiles[start.y][start.x + 1] = Tile::Wall;

    for row in &tiles {
        for tile in row {
            print!("{:?}", tile);
        }
        println!();
    }

    calculate(&tiles, start_positions, num_keys);
}

fn add_paths_from(
    from: Pos,
    tiles: &Vec<Vec<Tile>>,
    num_keys: usize,
    paths: &mut Vec<Vec<(Pos, i32, BitVec)>>,
) {
    let mut visited: HashSet<Pos> = HashSet::new();
    visited.insert(from);
    let mut queue: Vec<(i32, Pos, BitVec)> = Vec::new();
    queue.push((0, from, BitVec::from_elem(num_keys, false)));
    while let Some((dist, pos, keys)) = queue.pop() {
        for dir in &vec![Dir::North, Dir::East, Dir::South, Dir::West] {
            let new_pos = pos.go_dir(*dir);
            if !visited.contains(&new_pos) {
                let mut current_keys = keys.clone();
                match tiles[new_pos.y][new_pos.x] {
                    Tile::Wall => continue,
                    Tile::Door(key) => {
                        current_keys.set(key as usize, true);
                    }
                    Tile::Key(_) => {
                        paths[tiles[0].len() * from.y + from.x].push((
                            new_pos,
                            dist + 1,
                            current_keys.clone(),
                        ));
                    }
                    _ => (),
                }
                visited.insert(new_pos);
                queue.push((dist + 1, new_pos, current_keys.clone()));
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
struct Path {
    path_length: i32,
    positions: Vec<Pos>,
    keys: BitVec,
}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.path_length.cmp(&other.path_length))
    }
}

impl Ord for Path {
    fn cmp(&self, other: &Self) -> Ordering {
        self.path_length.cmp(&other.path_length)
    }
}

impl Path {
    pub fn new(path_length: i32, positions: Vec<Pos>, keys: BitVec) -> Self {
        Self {
            path_length,
            positions,
            keys,
        }
    }
}

fn calculate(tiles: &Vec<Vec<Tile>>, start_positions: Vec<Pos>, num_keys: usize) {
    let mut all_paths: Vec<Vec<(Pos, i32, BitVec)>> = vec![vec![]; tiles.len() * tiles[0].len()];
    for y in 0..tiles.len() {
        for x in 0..tiles[0].len() {
            match tiles[y][x] {
                Tile::Start | Tile::Key(_) => {
                    add_paths_from(Pos::new(x, y), tiles, num_keys, &mut all_paths)
                }
                _ => (),
            }
        }
    }
    for paths in &mut all_paths {
        paths.sort_by(|p1, p2| p2.1.partial_cmp(&p1.1).unwrap());
    }

    let mut queue: Vec<Path> = Vec::new();
    queue.push(Path::new(
        0,
        start_positions.clone(),
        BitVec::from_elem(num_keys, false),
    ));
    let mut shortest = std::i32::MAX;
    let mut shortest_paths: HashMap<(Vec<u32>, BitVec), i32> = HashMap::new();
    while let Some(Path {
        path_length,
        positions,
        keys,
    }) = queue.pop()
    {
        for (pos_index, pos) in positions.iter().enumerate() {
            for (to, dist, needed_keys) in &all_paths[tiles[0].len() * pos.y + pos.x] {
                if path_length + dist >= shortest {
                    continue;
                }

                let target = tiles[to.y][to.x].to_key().unwrap() as usize;
                if keys[target] {
                    continue;
                }
                if needed_keys
                    .iter()
                    .zip(&keys)
                    .any(|(needed, has_key)| needed && !has_key)
                {
                    continue;
                }

                let mut new_keys = keys.clone();
                new_keys.set(target, true);

                let mut new_positions = positions.clone();
                new_positions[pos_index] = *to;
                let key_indices: Vec<u32> = new_positions
                    .iter()
                    .map(|pos| tiles[pos.y][pos.x].to_key().unwrap_or(100))
                    .collect();

                if let Some(other_path_length) =
                    shortest_paths.get_mut(&(key_indices.clone(), new_keys.clone()))
                {
                    if *other_path_length < path_length + dist {
                        continue;
                    }
                    *other_path_length = path_length + dist;
                } else {
                    shortest_paths.insert((key_indices, new_keys.clone()), path_length + dist);
                }

                if new_keys.all() {
                    println!("length={}", path_length + dist);
                    shortest = path_length + dist;
                }
                queue.push(Path::new(path_length + dist, new_positions, new_keys));
            }
        }
    }
    println!("Answer = {}", shortest);
}
