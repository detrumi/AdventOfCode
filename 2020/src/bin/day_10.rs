use itertools::sorted;

const INPUT: &str = include_str!("../../input/day_10.txt");

fn parse() -> Vec<usize> {
    sorted(INPUT.lines().map(|line| line.parse().unwrap())).collect()
}

fn part1() -> usize {
    let numbers = parse();

    let mut current = 0;
    let mut differences = vec![0, 0, 1];
    for n in numbers {
        let diff = n - current;
        differences[diff - 1] += 1;
        current = n;
    }
    differences[0] * differences[2]
}

fn part2() -> usize {
    let numbers = parse();

    let mut ways = vec![(0, 1)];
    for n in numbers {
        let new_ways = ways
            .iter()
            .rev()
            .take_while(|(m, _)| m + 3 < n)
            .map(|(_, count)| count)
            .sum();
        if new_ways > 0 {
            ways.push((n, new_ways));
        }
    }
    ways.last().unwrap().1
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
