const INPUT: &str = include_str!("../../input/day_1.txt");

fn parse() -> impl Iterator<Item = i32> {
    INPUT.trim().lines().map(|line| {
        let delta = if line.chars().next().unwrap() == 'L' {
            -1
        } else {
            1
        };
        delta * line[1..].parse::<i32>().unwrap()
    })
}

fn part1() -> usize {
    let mut value = 50;
    let mut result = 0;
    for n in parse() {
        value += n;
        value %= 100;
        if value == 0 {
            result += 1;
        }
    }
    result
}

fn part2() -> usize {
    let mut value = 50;
    let mut result = 0;
    for n in parse() {
        for _ in 0..n.abs() {
            value = (value + n.signum() + 100) % 100;
            if value == 0 {
                result += 1;
            }
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
