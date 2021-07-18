const INPUT: &str = include_str!("../../input/day_15.txt");

fn parse() -> Vec<Vec<i64>> {
    INPUT
        .lines()
        .map(|line| {
            line.split(": ")
                .nth(1)
                .unwrap()
                .split(", ")
                .map(|s| s.split_ascii_whitespace().nth(1).unwrap().parse().unwrap())
                .collect()
        })
        .collect()
}

fn part1() -> i64 {
    let mults = parse();
    let mut best = i64::MIN;
    for a in 0_i64..=100 {
        for b in 0_i64..=100 - a {
            for c in 0_i64..=100 - a - b {
                let d = 100 - a - b - c;
                let amounts = vec![a, b, c, d];
                let score: i64 = (0..4)
                    .map(|i| {
                        mults
                            .iter()
                            .enumerate()
                            .map(|(row_index, row)| amounts[row_index] * row[i])
                            .sum::<i64>()
                            .max(0)
                    })
                    .product();
                best = best.max(score);
            }
        }
    }
    best
}

fn part2() -> i64 {
    let mults = parse();
    let mut best = i64::MIN;
    for a in 0_i64..=100 {
        for b in 0_i64..=100 - a {
            for c in 0_i64..=100 - a - b {
                let d = 100 - a - b - c;
                let amounts = vec![a, b, c, d];
                if (0..4)
                    .map(|i| (amounts[i] * mults[i][4]).max(0))
                    .sum::<i64>()
                    != 500
                {
                    continue;
                }

                let score: i64 = (0..4)
                    .map(|i| {
                        mults
                            .iter()
                            .enumerate()
                            .map(|(row_index, row)| amounts[row_index] * row[i])
                            .sum::<i64>()
                            .max(0)
                    })
                    .product();
                best = best.max(score);
            }
        }
    }
    best
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
