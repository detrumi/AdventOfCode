const INPUT: &str = include_str!("../../input/day_9.txt");
const LENGTH: usize = 25;

fn parse() -> Vec<usize> {
    INPUT
        .lines()
        .map(|line| line.parse::<usize>().unwrap())
        .collect()
}

fn part1() -> usize {
    let numbers = parse();
    'outer: for (i, &n) in numbers.iter().enumerate().skip(LENGTH) {
        for a in i - LENGTH..i - 1 {
            for b in a + 1..i {
                if numbers[a] + numbers[b] == n {
                    continue 'outer;
                }
            }
        }

        return n;
    }
    unreachable!()
}

fn part2() -> usize {
    let numbers = parse();
    let invalid = part1();
    for a in 0..numbers.len() - 1 {
        for b in a + 1..numbers.len() {
            if numbers[a..=b].iter().sum::<usize>() == invalid {
                return numbers[a..=b].iter().min().unwrap() + numbers[a..=b].iter().max().unwrap();
            }
        }
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
