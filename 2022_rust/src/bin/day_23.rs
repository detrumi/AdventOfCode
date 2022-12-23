use std::collections::{HashMap, HashSet, VecDeque};

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_23.txt");

type Pos = (isize, isize);

fn parse() -> HashSet<Pos> {
    INPUT
        .trim()
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .filter_map(move |(x, c)| (c == '#').then_some((x as isize, y as isize)))
        })
        .collect()
}

fn part2(max_rounds: isize) -> isize {
    let mut elves = parse();
    let mut directions: VecDeque<usize> = (0..4).collect();
    for round in 1..=max_rounds {
        let mut moves: HashMap<Pos, Vec<Pos>> = HashMap::new();
        for (x, y) in &elves {
            if (-1..=1)
                .cartesian_product(-1..=1)
                .filter(|(dx, dy)| *dx != 0 || *dy != 0)
                .all(|(dx, dy)| !elves.contains(&(x + dx, y + dy)))
            {
                continue;
            }

            let mut dest = None;
            for i in 0..4 {
                if let Some(dest2) = match directions[i] {
                    0 => (-1..=1)
                        .all(|dx| !elves.contains(&(x + dx, y - 1)))
                        .then_some((*x, y - 1)),
                    1 => (-1..=1)
                        .all(|dx| !elves.contains(&(x + dx, y + 1)))
                        .then_some((*x, y + 1)),
                    2 => (-1..=1)
                        .all(|dy| !elves.contains(&(x - 1, y + dy)))
                        .then_some((x - 1, *y)),
                    _ => (-1..=1)
                        .all(|dy| !elves.contains(&(x + 1, y + dy)))
                        .then_some((x + 1, *y)),
                } {
                    dest = Some(dest2);
                    break;
                }
            }

            if let Some(to) = dest {
                moves.entry(to).or_default().push((*x, *y));
            }
        }

        let first = directions.pop_front().unwrap();
        directions.push_back(first);

        let mut did_move = false;
        for (to, starts) in moves {
            if starts.len() == 1 {
                did_move = true;
                elves.remove(&starts[0]);
                elves.insert(to);
            }
        }
        if !did_move {
            return round;
        }
    }

    let (min_x, max_x) = elves.iter().map(|p| p.0).minmax().into_option().unwrap();
    let (min_y, max_y) = elves.iter().map(|p| p.1).minmax().into_option().unwrap();
    (max_x - min_x + 1) * (max_y - min_y + 1) - elves.len() as isize
}

fn main() {
    println!("Part 1: {}", part2(10));
    println!("Part 2: {}", part2(isize::MAX));
}
