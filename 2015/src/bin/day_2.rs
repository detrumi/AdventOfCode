const INPUT: &str = include_str!("../../input/day_2.txt");

fn parse_input() -> Vec<Vec<u32>> {
    INPUT
        .lines()
        .map(|line| line.split('x').map(|n| n.parse::<u32>().unwrap()).collect())
        .collect()
}

fn part1() -> u32 {
    parse_input()
        .iter()
        .map(|sides| {
            let a = sides[0] * sides[1];
            let b = sides[1] * sides[2];
            let c = sides[0] * sides[2];
            2 * (a + b + c) + a.min(b).min(c)
        })
        .sum()
}

fn part2() -> u32 {
    parse_input()
        .iter_mut()
        .map(|sides| {
            sides.sort();
            2 * (sides[0] + sides[1]) + sides.iter().product::<u32>()
        })
        .sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
