const INPUT: &str = include_str!("../../input/day_8.txt");

fn part1() -> usize {
    let mut literals = 0;
    for line in INPUT.lines() {
        let mut backslash = false;
        let mut to_escape = 0;
        for c in line.chars() {
            match c {
                '"' => {
                    if backslash {
                        backslash = false
                    } else {
                        literals += 1
                    }
                }
                '\\' => {
                    if backslash {
                        backslash = false
                    } else {
                        backslash = true;
                        literals += 1
                    }
                }
                'x' if backslash => {
                    backslash = false;
                    to_escape = 2;
                    literals += 2;
                }
                _ if to_escape > 0 => {
                    to_escape -= 1;
                }
                _ => (),
            }
        }
    }
    literals
}

fn part2() -> usize {
    INPUT
        .lines()
        .map(|line| 2 + line.chars().filter(|&c| c == '"' || c == '\\').count())
        .sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
