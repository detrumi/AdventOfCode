const INPUT: &str = include_str!("../../input/day_25.txt");

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Dir {
    East,
    South,
}

fn parse() -> Vec<Vec<Option<Dir>>> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    '.' => None,
                    '>' => Some(Dir::East),
                    'v' => Some(Dir::South),
                    _ => panic!(),
                })
                .collect()
        })
        .collect()
}

fn move_dir(dir: Dir, cucumbers: Vec<Vec<Option<Dir>>>) -> (Vec<Vec<Option<Dir>>>, bool) {
    let mut result = cucumbers.clone();
    let mut moved = false;
    for y in 0..cucumbers.len() {
        for x in 0..cucumbers[0].len() {
            if cucumbers[y][x] == Some(dir) {
                let (y2, x2) = match dir {
                    Dir::East => (y, (x + 1) % cucumbers[0].len()),
                    Dir::South => ((y + 1) % cucumbers.len(), x),
                };
                if cucumbers[y2][x2].is_none() {
                    result[y2][x2] = Some(dir);
                    result[y][x] = None;
                    moved = true;
                }
            }
        }
    }
    (result, moved)
}

fn part1() -> usize {
    let mut cucumbers = parse();
    for step in 1.. {
        let (cucumbers1, moved_east) = move_dir(Dir::East, cucumbers);
        let (cucumbers2, moved_south) = move_dir(Dir::South, cucumbers1);
        cucumbers = cucumbers2;
        if !moved_east && !moved_south {
            return step;
        }
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", part1());
}
