const INPUT: &str = include_str!("../../input/day_11.txt");

fn parse() -> Vec<Vec<char>> {
    INPUT.lines().map(|line| line.chars().collect()).collect()
}

fn next(y: i32, x: i32, input: &Vec<Vec<char>>) -> char {
    let mut occupied = 0;
    for (dy, dx) in &[
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ] {
        if y + dy >= 0
            && y + dy < input.len() as i32
            && x + dx >= 0
            && x + dx < input[0].len() as i32
        {
            occupied += if input[(y + dy) as usize][(x + dx) as usize] == '#' {
                1
            } else {
                0
            };
        }
    }
    let current = input[y as usize][x as usize];
    match current {
        'L' if occupied == 0 => '#',
        '#' if occupied >= 4 => 'L',
        _ => current,
    }
}

fn part1() -> usize {
    let mut input = parse();
    for _round in 0.. {
        let mut new = input.clone();
        for y in 0..new.len() {
            for x in 0..new[0].len() {
                new[y][x] = next(y as i32, x as i32, &input)
            }
        }
        if input == new {
            break;
        }
        input = new.clone();
    }
    input
        .iter()
        .map(|line| line.iter().filter(|&c| *c == '#').count())
        .sum()
}

fn next_visible(y: i32, x: i32, input: &Vec<Vec<char>>) -> char {
    let mut occupied = 0;
    for (dy, dx) in &[
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ] {
        for distance in 1.. {
            let new_y = y + distance * dy;
            let new_x = x + distance * dx;
            if new_y < 0
                || new_y >= input.len() as i32
                || new_x < 0
                || new_x >= input[0].len() as i32
            {
                break;
            }

            if input[(new_y) as usize][(new_x) as usize] == '#' {
                occupied += 1;
                break;
            } else if input[(new_y) as usize][(new_x) as usize] == 'L' {
                break;
            }
        }
    }
    let current = input[y as usize][x as usize];
    match current {
        'L' if occupied == 0 => '#',
        '#' if occupied >= 5 => 'L',
        _ => current,
    }
}

fn part2() -> usize {
    let mut input: Vec<Vec<_>> = parse();
    for _round in 0.. {
        let mut new = input.clone();
        for y in 0..new.len() {
            for x in 0..new[0].len() {
                new[y][x] = next_visible(y as i32, x as i32, &input)
            }
        }
        if input == new {
            break;
        }
        input = new.clone();
    }
    input
        .iter()
        .map(|line| line.iter().filter(|&c| *c == '#').count())
        .sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
