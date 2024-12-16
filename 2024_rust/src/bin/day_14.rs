use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_14.txt");

fn parse() -> Vec<(i32, i32, i32, i32)> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            line[2..]
                .split(" v=")
                .flat_map(|t| t.split(","))
                .map(|n| n.parse::<i32>().unwrap())
                .collect_tuple()
                .unwrap()
        })
        .collect()
}

fn part1() -> usize {
    let (x_max, y_max) = (101, 103);
    let mut quadrants = [0; 4];
    for (mut x, mut y, vx, vy) in parse() {
        for _ in 0..100 {
            x = (x + vx + x_max) % x_max;
            y = (y + vy + y_max) % y_max;
        }
        if let Some(quadrant) = match (x, y) {
            _ if x < x_max / 2 && y < y_max / 2 => Some(0),
            _ if x > x_max / 2 && y < y_max / 2 => Some(1),
            _ if x < x_max / 2 && y > y_max / 2 => Some(2),
            _ if x > x_max / 2 && y > y_max / 2 => Some(3),
            _ => None,
        } {
            quadrants[quadrant] += 1;
        }
    }
    quadrants.iter().product()
}

fn part2() -> usize {
    let (x_max, y_max) = (101, 103);
    let mut robots = parse();
    for t in 1.. {
        for (x, y, vx, vy) in &mut robots {
            *x = (*x + *vx + x_max) % x_max;
            *y = (*y + *vy + y_max) % y_max;
        }
        if robots
            .iter()
            .filter(|(_, ry, _, _)| *ry > 26 && *ry < 61)
            .count()
            > 200
            && robots
                .iter()
                .filter(|(rx, _, _, _)| *rx > 20 && *rx < x_max - 20)
                .count()
                > 400
        {
            return t;
        }
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
