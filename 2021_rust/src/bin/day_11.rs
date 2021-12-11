const INPUT: &str = include_str!("../../input/day_11.txt");

#[derive(Clone, Copy, Debug)]
struct Pos {
    pub x: isize,
    pub y: isize,
}

impl Pos {
    pub fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }
}

#[derive(Default)]
struct Map {
    levels: Vec<Vec<usize>>,
    flashes: Vec<Pos>,
    height: isize,
    width: isize,
    total_flashes: usize,
}

impl Map {
    pub fn new() -> Self {
        let levels: Vec<Vec<usize>> = INPUT
            .trim()
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| c.to_digit(10).unwrap() as usize)
                    .collect()
            })
            .collect();

        Self {
            height: levels.len() as isize,
            width: levels[0].len() as isize,
            levels,
            ..Default::default()
        }
    }

    fn increase(&mut self, pos: Pos) -> bool {
        let level = &mut self.levels[pos.y as usize][pos.x as usize];
        if *level > 9 {
            return false;
        }
        *level += 1;
        *level == 10
    }

    fn flashes(&mut self) {
        for y in 0..self.height {
            for x in 0..self.width {
                let pos = Pos::new(x, y);
                if self.increase(pos) {
                    self.flashes.push(pos);
                    self.total_flashes += 1;
                }
            }
        }

        while self.flashes.len() > 0 {
            let flashes = self.flashes.clone();
            self.flashes.clear();
            for pos in flashes {
                for neighbor in self.neighbors(pos) {
                    if self.increase(neighbor) {
                        self.flashes.push(neighbor);
                        self.total_flashes += 1;
                    }
                }
            }
        }

        for y in 0..self.height {
            for x in 0..self.width {
                let level = &mut self.levels[y as usize][x as usize];
                if *level > 9 {
                    *level = 0;
                }
            }
        }
    }

    fn neighbors(&mut self, pos: Pos) -> Vec<Pos> {
        let mut neighbors = vec![];
        for dy in -1..=1 {
            for dx in -1..=1 {
                if pos.y + dy >= 0
                    && pos.y + dy < self.height
                    && pos.x + dx >= 0
                    && pos.x + dx < self.width
                {
                    neighbors.push(Pos::new(pos.x + dx, pos.y + dy));
                }
            }
        }
        neighbors
    }
}

fn part1() -> usize {
    let mut map = Map::new();
    for _step in 0..100 {
        map.flashes();
    }
    map.total_flashes
}

fn part2() -> usize {
    let mut map = Map::new();
    let mut old_total = 0;
    for step in 1.. {
        map.flashes();
        let num_flashes = map.total_flashes - old_total;
        if num_flashes as isize == map.width * map.height {
            return step;
        }
        old_total = map.total_flashes;
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
