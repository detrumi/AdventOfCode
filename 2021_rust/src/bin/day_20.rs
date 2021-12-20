use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_20.txt");

#[derive(Clone, Copy, PartialEq, Eq, Default, Hash, Debug)]
struct Pos {
    pub x: isize,
    pub y: isize,
}

impl Pos {
    pub fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }

    fn neighbors(self) -> impl Iterator<Item = Pos> {
        (-1..=1).flat_map(move |dy| (-1..=1).map(move |dx| Pos::new(self.x + dx, self.y + dy)))
    }
}

fn solve(rounds: usize) -> usize {
    let parts: Vec<_> = INPUT.trim().split("\n\n").collect();
    let algorithm: Vec<bool> = parts[0].chars().map(|c| c == '#').collect();
    let mut image: HashSet<Pos> = parts[1]
        .lines()
        .enumerate()
        .flat_map(move |(y, line)| {
            line.chars()
                .enumerate()
                .filter(|(_x, c)| *c == '#')
                .map(move |(x, _c)| Pos::new(x as isize, y as isize))
        })
        .collect();

    for _enhancement in 0..rounds / 2 {
        let y_min = image.iter().min_by_key(|p| p.y).unwrap().y - 3;
        let y_max = image.iter().max_by_key(|p| p.y).unwrap().y + 3;
        let x_min = image.iter().min_by_key(|p| p.x).unwrap().x - 3;
        let x_max = image.iter().max_by_key(|p| p.x).unwrap().x + 3;
        for _subround in 0..2 {
            let mut new_image = HashSet::new();
            for y in y_min..=y_max {
                for x in x_min..=x_max {
                    let pos = Pos::new(x, y);
                    let s: String = pos
                        .neighbors()
                        .map(|n| if image.contains(&n) { '1' } else { '0' })
                        .collect();
                    if algorithm[usize::from_str_radix(&s, 2).unwrap()] {
                        new_image.insert(pos);
                    }
                }
            }
            image = new_image;
        }

        image.retain(|p| p.y > y_min && p.y < y_max && p.x > x_min && p.x < x_max);
    }

    image.len()
}

fn main() {
    println!("Part 1: {}", solve(2));
    println!("Part 2: {}", solve(50));
}
