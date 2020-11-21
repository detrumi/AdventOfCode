use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_5.txt");

fn is_nice(s: &str) -> bool {
    let mut twice = false;
    for (a, b) in s.chars().tuple_windows() {
        if a == b {
            twice = true;
        }

        if [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')].contains(&(a, b)) {
            return false;
        }
    }

    twice && s.chars().filter(|c| "aeiou".contains(*c)).count() >= 3
}

fn is_nicer(s: &str) -> bool {
    let chars: Vec<char> = s.chars().collect();

    let mut found_pair = false;
    for i in 0..s.len() - 2 {
        if s[i + 2..].contains(&s[i..i + 2]) {
            found_pair = true;
        }
    }

    let repeats = (0..s.len() - 2).any(|i| chars[i] == chars[i + 2]);

    found_pair && repeats
}

fn part1() -> usize {
    INPUT.lines().filter(|line| is_nice(line)).count()
}

fn part2() -> usize {
    INPUT.lines().filter(|line| is_nicer(line)).count()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
