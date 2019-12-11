use std::collections::HashSet;
use std::f32::consts::PI;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Default)]
struct Pos {
    x: i32,
    y: i32,
}

impl Pos {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    pub fn in_bounds(self, width: usize, height: usize) -> bool {
        self.x >= 0 && self.y >= 0 && self.x < width as i32 && self.y < height as i32
    }

    pub fn angle_to(self, other: Pos) -> f32 {
        let angle = f32::atan2((other.y - self.y) as f32, (other.x - self.x) as f32);
        (angle + 2.5 * PI) % (2.0 * PI)
    }
}

fn main() {
    let obs = part1();
    part2(obs);
}

fn part1() -> Pos {
    let file = File::open("input/day_10.txt").unwrap();
    let lines: Vec<_> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap().chars().collect::<Vec<_>>())
        .collect();
    let width = lines[0].len();
    let height = lines.len();
    let num_asteroids = lines
        .iter()
        .map(|row| row.iter().filter(|&c| *c == '#').count())
        .sum::<usize>();
    let mut best = std::usize::MIN;
    let mut best_asteroid = Pos::default();
    for y in 0..height {
        for x in 0..width {
            if lines[y][x] == '#' {
                let mut blocked: HashSet<Pos> = HashSet::new();
                for y1 in 0..height {
                    for x1 in 0..width {
                        let p1 = Pos::new(x1 as i32, y1 as i32);
                        if (p1.x == x as i32 && p1.y == y as i32)
                            || !p1.in_bounds(width, height)
                            || lines[p1.y as usize][p1.x as usize] != '#'
                        {
                            continue;
                        }

                        let mut dx = x1 as i32 - x as i32;
                        let mut dy = y1 as i32 - y as i32;
                        let gcd = num_integer::gcd(dx, dy);
                        dx /= gcd;
                        dy /= gcd;
                        for d in 2.. {
                            let p2 = Pos::new(x as i32 + d * dx, y as i32 + d * dy);
                            if !p2.in_bounds(width, height) || p1 == p2 {
                                break;
                            }
                            if lines[p2.y as usize][p2.x as usize] == '#' {
                                blocked.insert(p2);
                            }
                        }
                    }
                }
                let result = num_asteroids - 1 - blocked.len();
                if result > best {
                    best = result;
                    best_asteroid = Pos::new(x as i32, y as i32);
                }
            }
        }
    }
    eprintln!("Part 1 = {:?}", best);
    best_asteroid
}

fn part2(obs: Pos) {
    let file = File::open("input/day_10.txt").unwrap();
    let mut lines: Vec<_> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap().chars().collect::<Vec<_>>())
        .collect();

    let mut num_vaporized = 0;
    loop {
        let mut in_sight: Vec<(f32, Pos)> = in_sight(obs, &lines)
            .iter()
            .map(|p| (obs.angle_to(*p), *p))
            .collect();
        in_sight.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
        for (_dir, pos) in &in_sight {
            num_vaporized += 1;
            if num_vaporized == 200 {
                eprintln!("Part 2 = {:?} => {}", pos, 100 * pos.x as i32 + pos.y);
                return;
            }
            lines[pos.y as usize][pos.x as usize] = '.';
        }
    }
}

fn in_sight(obs: Pos, lines: &[Vec<char>]) -> HashSet<Pos> {
    let mut in_sight: HashSet<Pos> = HashSet::new();

    let width = lines[0].len();
    let height = lines.len();
    for y in 0..height {
        for x in 0..width {
            if lines[y][x] == '#' && (x != obs.x as usize || y != obs.y as usize) {
                in_sight.insert(Pos::new(x as i32, y as i32));
            }
        }
    }

    for y1 in 0..height {
        for x1 in 0..width {
            let p1 = Pos::new(x1 as i32, y1 as i32);
            if (p1.x == obs.x && p1.y == obs.y)
                || !p1.in_bounds(width, height)
                || lines[p1.y as usize][p1.x as usize] != '#'
            {
                continue;
            }

            let mut dx = x1 as i32 - obs.x;
            let mut dy = y1 as i32 - obs.y;
            let gcd = num_integer::gcd(dx, dy);
            dx /= gcd;
            dy /= gcd;
            for d in 2.. {
                let p2 = Pos::new(obs.x + d * dx, obs.y + d * dy);
                if !p2.in_bounds(width, height) || p1 == p2 {
                    break;
                }
                if lines[p2.y as usize][p2.x as usize] == '#' {
                    in_sight.remove(&p1);
                }
            }
        }
    }
    in_sight
}
