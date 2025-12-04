const INPUT: &str = include_str!("../../input/day_4.txt");

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
        (-1..=1).flat_map(move |dy| {
            (-1..=1)
                .filter(move |dx| *dx != 0 || dy != 0)
                .map(move |dx| Pos::new(self.x + dx, self.y + dy))
        })
    }
}

fn parse() -> Vec<Vec<bool>> {
    INPUT
        .trim()
        .lines()
        .map(|line| line.chars().map(|c| c == '@').collect())
        .collect()
}

fn part1() -> usize {
    remove(&mut parse())
}

fn remove(map: &mut Vec<Vec<bool>>) -> usize {
    let mut removed = 0;
    let old_map = map.clone();
    for (y, row) in old_map.iter().enumerate() {
        for (x, _) in row.iter().enumerate().filter(|(_, cell)| **cell) {
            let count = Pos::new(x as isize, y as isize)
                .neighbors()
                .filter(|pos| {
                    (0..old_map.len() as isize).contains(&pos.y)
                        && (0..old_map[0].len() as isize).contains(&pos.x)
                })
                .filter(|pos| old_map[pos.y as usize][pos.x as usize])
                .count();
            if count < 4 {
                map[y][x] = false;
                removed += 1;
            }
        }
    }
    removed
}

fn part2() -> usize {
    let mut result = 0;
    let mut map = parse();
    loop {
        let removed = remove(&mut map);
        if removed == 0 {
            return result;
        }
        result += removed;
    }
}

fn main() {
    println!("Part 1: {}", part1()); // 1064 wrong
    println!("Part 2: {}", part2());
}
