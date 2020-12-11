const INPUT: &str = include_str!("../../input/day_11.txt");

fn parse() -> Vec<Vec<char>> {
    INPUT.lines().map(|line| line.chars().collect()).collect()
}

fn next(y: i32, x: i32, input: &Vec<Vec<char>>, max_distance: i32, min_seats: usize) -> char {
    let mut occupied = 0;
    for dy in -1..=1 {
        for dx in (-1..=1).filter(|&dx| dy != 0 || dx != 0) {
            for distance in 1..=max_distance {
                match input
                    .get((y + distance * dy) as usize)
                    .and_then(|row| row.get((x + distance * dx) as usize))
                {
                    None => break,
                    Some('#') => {
                        occupied += 1;
                        break;
                    }
                    Some('L') => break,
                    _ => (),
                }
            }
        }
    }
    match input[y as usize][x as usize] {
        'L' if occupied == 0 => '#',
        '#' if occupied >= min_seats => 'L',
        current => current,
    }
}

fn solve(max_distance: i32, min_seats: usize) -> usize {
    let mut grid = parse();
    for _round in 0.. {
        let mut new = grid.clone();
        for y in 0..new.len() {
            for x in 0..new[0].len() {
                new[y][x] = next(y as i32, x as i32, &grid, max_distance, min_seats)
            }
        }
        if grid == new {
            break;
        }
        grid = new.clone();
    }
    grid.iter()
        .flat_map(|line| line.iter().filter(|&c| *c == '#'))
        .count()
}

fn main() {
    println!("Part 1: {}", solve(1, 4));
    println!("Part 2: {}", solve(1000, 5));
}
