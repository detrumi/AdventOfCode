const INPUT: &str = include_str!("../../input/day_7.txt");

fn parse() -> Vec<(usize, Vec<usize>)> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let (total, numbers) = line.split_once(": ").unwrap();
            let total = total.parse().unwrap();
            let numbers = numbers
                .split_ascii_whitespace()
                .map(|n| n.parse().unwrap())
                .rev()
                .collect();
            (total, numbers)
        })
        .collect()
}

fn part1() -> usize {
    parse()
        .into_iter()
        .filter(|(total, numbers)| totals(&numbers).contains(total))
        .map(|(total, _)| total)
        .sum()
}

fn totals(numbers: &[usize]) -> Vec<usize> {
    if numbers.len() == 1 {
        vec![numbers[0]]
    } else {
        totals(&numbers[1..])
            .into_iter()
            .flat_map(|rest| [numbers[0] + rest, numbers[0] * rest].into_iter())
            .collect()
    }
}

fn part2() -> usize {
    parse()
        .into_iter()
        .filter(|(total, numbers)| totals2(&numbers).contains(total))
        .map(|(total, _)| total)
        .sum()
}

fn totals2(numbers: &[usize]) -> Vec<usize> {
    if numbers.len() == 1 {
        vec![numbers[0]]
    } else {
        totals2(&numbers[1..])
            .into_iter()
            .flat_map(|rest| {
                [
                    numbers[0] + rest,
                    numbers[0] * rest,
                    format!("{}{}", rest, numbers[0]).parse().unwrap(),
                ]
                .into_iter()
            })
            .collect()
    }
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
