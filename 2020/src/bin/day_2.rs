const INPUT: &str = include_str!("../../input/day_2.txt");

fn part1() -> usize {
    let mut result = 0;
    for line in INPUT.lines() {
        let words: Vec<_> = line.split_ascii_whitespace().collect();
        let lengths: Vec<_> = words[0]
            .split('-')
            .map(|n| n.parse::<usize>().unwrap())
            .collect();
        let name = words[1].trim_end_matches(':');
        let pass = words[2];

        let count = pass
            .chars()
            .filter(|&c| c == name.chars().next().unwrap())
            .count();
        if count >= lengths[0] && count <= lengths[1] {
            result += 1;
        }
    }

    result
}

fn part2() -> usize {
    let mut result = 0;
    for line in INPUT.lines() {
        let words: Vec<_> = line.split_ascii_whitespace().collect();
        let lengths: Vec<_> = words[0]
            .split('-')
            .map(|n| n.parse::<usize>().unwrap())
            .collect();
        let name = words[1].trim_end_matches(':').chars().next().unwrap();
        let pass: Vec<_> = words[2].chars().collect();

        if (pass[lengths[0] - 1] == name) != (pass[lengths[1] - 1] == name) {
            result += 1;
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
