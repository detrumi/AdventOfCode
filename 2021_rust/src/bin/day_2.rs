const INPUT: &str = include_str!("../../input/day_2.txt");

fn parse() -> impl Iterator<Item = (&'static str, isize)> {
    INPUT.trim().lines().map(|line| {
        let parts: Vec<_> = line.split_ascii_whitespace().collect();
        (parts[0], parts[1].parse().unwrap())
    })
}

#[derive(Default, Debug)]
struct Pos {
    pub x: isize,
    pub y: isize,
}

fn part1() -> isize {
    let mut pos = Pos::default();
    for (command, n) in parse() {
        match command {
            "forward" => pos.x += n,
            "down" => pos.y += n,
            "up" => pos.y -= n,
            _ => panic!(),
        }
    }
    pos.x * pos.y
}

fn part2() -> isize {
    let mut pos = Pos::default();
    let mut aim = 0;
    for (command, n) in parse() {
        match command {
            "forward" => {
                pos.x += n;
                pos.y += n * aim;
            }
            "down" => aim += n,
            "up" => aim -= n,
            _ => panic!(),
        }
    }
    pos.x * pos.y
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
