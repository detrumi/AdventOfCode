use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_13.txt");

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Pos {
    pub x: usize,
    pub y: usize,
}

impl Pos {
    pub fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }
}

fn parse() -> (Vec<Pos>, Vec<(String, usize)>) {
    let parts: Vec<_> = INPUT.trim().split("\n\n").collect();
    let coords: Vec<_> = parts[0]
        .lines()
        .map(|line| {
            let (x, y) = line.split_once(',').unwrap();
            Pos::new(x.parse().unwrap(), y.parse().unwrap())
        })
        .collect();

    let folds: Vec<(String, usize)> = parts[1]
        .lines()
        .map(|line| {
            let (side, n) = line
                .split_ascii_whitespace()
                .nth(2)
                .unwrap()
                .split_once('=')
                .unwrap();
            (side.to_string(), n.parse().unwrap())
        })
        .collect();

    (coords, folds)
}

fn fold(mut points: HashSet<Pos>, folds: Vec<(String, usize)>) -> HashSet<Pos> {
    for (side, n) in &folds {
        let mut new_points: HashSet<Pos> = HashSet::new();
        for point in &points {
            let mut pos = point.clone();
            if side == "y" && point.y > *n {
                pos.y -= 2 * (point.y - n);
            }
            if side == "x" && point.x > *n {
                pos.x -= 2 * (point.x - n);
            }
            new_points.insert(pos);
        }
        points = new_points;
    }
    points
}

fn main() {
    let (coords, folds) = parse();
    let points: HashSet<Pos> = coords.iter().cloned().collect();

    let first_fold = fold(points.clone(), vec![folds[0].clone()]);
    println!("Part 1: {}", first_fold.len());

    let points = fold(points, folds);
    let x_max = points.iter().max_by(|a, b| a.x.cmp(&b.x)).unwrap().x;
    let y_max = points.iter().max_by(|a, b| a.y.cmp(&b.y)).unwrap().y;
    println!("Part 2:");
    for y in 0..y_max {
        for x in 0..x_max {
            print!(
                "{}",
                if points.contains(&Pos::new(x, y)) {
                    '#'
                } else {
                    '.'
                }
            );
        }
        println!();
    }
}
