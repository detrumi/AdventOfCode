use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_5.txt");

#[derive(Default, PartialEq, Eq, Hash, Debug)]
struct Pos {
    pub x: isize,
    pub y: isize,
}

impl Pos {
    pub fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }

    fn parse(s: &str) -> Self {
        let (x, y) = s.trim().split_once(',').unwrap();
        Self::new(x.parse().unwrap(), y.parse().unwrap())
    }
}

fn parse() -> Vec<(Pos, Pos)> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let (from, to) = line.split_once(" -> ").unwrap();
            (Pos::parse(from), Pos::parse(to))
        })
        .collect()
}

fn count_overlap<'a>(lines: impl Iterator<Item = &'a (Pos, Pos)>) -> usize {
    let mut covered: HashMap<Pos, usize> = HashMap::new();
    for (from, to) in lines {
        let dir_x = (to.x - from.x).signum();
        let dir_y = (to.y - from.y).signum();
        for i in 0..=(to.x - from.x).abs().max((to.y - from.y).abs()) {
            let pos = Pos::new(from.x + i * dir_x, from.y + i * dir_y);
            *covered.entry(pos).or_default() += 1;
        }
    }
    covered.iter().filter(|&(_p, n)| *n > 1).count()
}

fn main() {
    let lines = parse();
    let straight_lines = lines
        .iter()
        .filter(|(from, to)| from.x == to.x || from.y == to.y);
    println!("Part 1: {}", count_overlap(straight_lines));
    println!("Part 2: {}", count_overlap(lines.iter()));
}
