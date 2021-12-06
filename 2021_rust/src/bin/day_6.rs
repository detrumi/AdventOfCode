const INPUT: &str = include_str!("../../input/day_6.txt");

fn parse() -> Vec<usize> {
    INPUT
        .trim()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1() -> usize {
    let mut timers = parse();
    for _day in 1..=80 {
        for i in 0..timers.len() {
            match timers[i] {
                0 => {
                    timers[i] = 6;
                    timers.push(8);
                }
                n => timers[i] = n - 1,
            }
        }
    }
    timers.len()
}

fn part2() -> usize {
    let timers = parse();
    let mut counts = [0_usize; 9];
    for timer in timers {
        counts[timer] += 1;
    }

    for _day in 1..=256 {
        let mut new_counts = [0_usize; 9];
        for i in 0..=7 {
            new_counts[i] = counts[i + 1];
        }
        new_counts[8] = counts[0];
        new_counts[6] += counts[0];
        counts = new_counts;
    }
    counts.iter().sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
