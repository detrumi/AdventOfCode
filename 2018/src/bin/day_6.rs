use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Default)]
struct Pos {
    x: i32,
    y: i32,
}

fn neighbors(p: Pos) -> Vec<Pos> {
    return vec![
        Pos { x: p.x + 1, y: p.y },
        Pos { x: p.x - 1, y: p.y },
        Pos { x: p.x, y: p.y + 1 },
        Pos { x: p.x, y: p.y - 1 },
    ];
}

fn main() {
    let file = File::open("input/day_6.txt").unwrap();
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .collect();

    part_1(&lines);
    part_2(&lines);
}

fn part_1(lines: &Vec<String>) {
    let mut points: VecDeque<(Pos, i32, i32)> = VecDeque::new(); // p, dist, color
    let mut i = 0;
    for line in lines {
        let parts: Vec<_> = line
            .split(", ")
            .map(|s| s.parse::<i32>().unwrap())
            .collect();
        let pos = Pos {
            x: parts[0],
            y: parts[1],
        };
        points.push_back((pos, 0, i));
        i += 1;
    }

    let mut colors: HashMap<Pos, (i32, Option<i32>)> = HashMap::new(); // dist, color
    let mut colors_temp: HashMap<Pos, (i32, Option<i32>)> = HashMap::new();
    while let Some((p, d, i)) = points.pop_front() {
        if colors.contains_key(&p) {
            if colors[&p].1.is_some() && colors[&p].1 != Some(i) && colors[&p].0 == d {
                colors.remove(&p);
                colors.insert(p, (d, None));
            }
            continue;
        }
        colors.insert(p, (d, Some(i)));
        for n in neighbors(p) {
            points.push_back((n, d + 1, i));
        }
        let limit = 200;
        if d == limit - 5 {
            colors_temp = colors.clone();
        } else if d == limit {
            break;
        }
    }

    let mut best = 0;
    for color in 0..lines.len() {
        let old = colors_temp
            .iter()
            .filter(|(_, i)| i.1 == Some(color as i32))
            .count();
        let new = colors
            .iter()
            .filter(|(_, i)| i.1 == Some(color as i32))
            .count();

        if old == new {
            best = best.max(old);
        }
    }
    println!("Part 1: {}", best);
}

fn part_2(lines: &Vec<String>) {
    let mut distances: HashMap<Pos, (i32, i32)> = HashMap::new(); // count, total distance
    for line in lines {
        let parts: Vec<_> = line
            .split(", ")
            .map(|s| s.parse::<i32>().unwrap())
            .collect();
        let pos = Pos {
            x: parts[0],
            y: parts[1],
        };

        let mut points: VecDeque<(Pos, i32)> = VecDeque::new(); // Pos -> distance

        points.push_back((pos, 0));
        let mut found: HashSet<Pos> = HashSet::new();
        while let Some((p, d)) = points.pop_front() {
            if d >= 500 {
                break;
            }

            if found.contains(&p) {
                continue;
            }
            found.insert(p);

            let (c, total) = distances.entry(p).or_default();
            *c += 1;
            *total += d;

            for n in neighbors(p) {
                points.push_back((n, d + 1));
            }
        }
    }
    println!(
        "Part 2: {}",
        distances
            .iter()
            .filter(|(_, (count, d))| *count as usize == lines.len() && *d < 10000)
            .count()
    );
}
