const INPUT: &str = include_str!("../../input/day_2.txt");

fn parse() -> Vec<Vec<isize>> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            line.split_ascii_whitespace()
                .map(|n| n.parse::<isize>().unwrap())
                .collect()
        })
        .collect()
}

fn check(report: &Vec<isize>) -> bool {
    let sign = (report[1] - report[0]).signum();
    sign != 0
        && report
            .windows(2)
            .all(|w| (w[1] - w[0]).signum() == sign && w[0].abs_diff(w[1]) <= 3)
}

fn part1() -> usize {
    parse().into_iter().filter(check).count()
}

fn part2() -> usize {
    parse()
        .into_iter()
        .filter(|report| {
            check(report)
                || (0..report.len()).any(|n| {
                    let mut r = report.clone();
                    r.remove(n);
                    check(&r)
                })
        })
        .count()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
