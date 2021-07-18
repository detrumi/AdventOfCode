const INPUT: &str = include_str!("../../input/day_1.txt");

fn follow(c: char) -> i32 {
    match c {
        '(' => 1,
        ')' => -1,
        _ => panic!(),
    }
}

fn part1() -> i32 {
    INPUT.chars().map(follow).sum()
}

fn part2() -> usize {
    let mut floor = 0;
    for (i, c) in INPUT.char_indices() {
        floor += follow(c);
        if floor == -1 {
            return i + 1;
        }
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
