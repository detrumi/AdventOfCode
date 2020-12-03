const INPUT: &str = include_str!("../../input/day_3.txt");

fn parse() -> (Vec<bool>, (usize, usize)) {
    let mut trees = vec![];

    let lines: Vec<_> = INPUT.lines().collect();
    let size = (lines[0].len(), lines.len() - 1);
    for line in lines {
        for c in line.chars() {
            trees.push(if c == '#' { true } else { false });
        }
    }
    (trees, size)
}

fn solve(trees: &[bool], size: (usize, usize), slope: (usize, usize)) -> usize {
    let mut count = 0;
    let mut pos = (0, 0);
    loop {
        if trees[size.0 * pos.1 + pos.0] {
            count += 1;
        }
        pos = ((pos.0 + slope.0) % size.0, pos.1 + slope.1);
        if pos.1 > size.1 {
            return count;
        }
    }
}

fn part1() -> usize {
    let (trees, size) = parse();
    solve(&trees, size, (3, 1))
}

fn part2() -> usize {
    let (trees, size) = parse();
    [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|&slope| solve(&trees, size, slope))
        .product()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
