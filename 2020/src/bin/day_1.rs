const INPUT: &str = include_str!("../../input/day_1.txt");

fn parse() -> Vec<usize> {
    INPUT.lines().map(|l| l.parse::<usize>().unwrap()).collect()
}

fn part1() -> usize {
    let input = parse();
    for i in 0..input.len() {
        for j in i + 1..input.len() {
            if input[i] + input[j] == 2020 {
                return input[i] * input[j];
            }
        }
    }
    0
}

fn part2() -> usize {
    let input = parse();
    for i in 0..input.len() {
        for j in i + 1..input.len() {
            for k in j + 1..input.len() {
                if input[i] + input[j] + input[k] == 2020 {
                    return input[i] * input[j] * input[k];
                }
            }
        }
    }
    0
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
