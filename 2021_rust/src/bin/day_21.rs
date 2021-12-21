const INPUT: &str = include_str!("../../input/day_21.txt");

fn parse() -> Vec<usize> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            line.split_ascii_whitespace()
                .last()
                .unwrap()
                .parse()
                .unwrap()
        })
        .collect()
}

fn part1() -> usize {
    let mut positions = parse();
    let mut scores = [0, 0];

    let mut die = 0;
    let mut num_rolls = 0;
    for _turn in 0.. {
        for player in 0..2 {
            let mut roll_total = 0;
            for _ in 0..3 {
                die = die % 100 + 1;
                num_rolls += 1;
                roll_total += die;
            }
            positions[player] += roll_total;
            positions[player] = (positions[player] - 1) % 10 + 1;
            scores[player] += positions[player];
            if scores[player] >= 1000 {
                return scores[1 - player] * num_rolls;
            }
        }
    }
    unreachable!()
}

fn simulate(positions: [usize; 2], scores: [usize; 2]) -> [usize; 2] {
    let mut result = [0, 0];
    for (roll_total, frequency) in [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)] {
        let mut pos = positions[0] + roll_total;
        pos = (pos - 1) % 10 + 1;
        let score = scores[0] + pos;
        if score >= 21 {
            result[0] += frequency;
        } else {
            let [a, b] = simulate([positions[1], pos], [scores[1], score]);
            result[0] += frequency * b;
            result[1] += frequency * a;
        }
    }
    result
}

fn part2() -> usize {
    let positions = parse();
    let result = simulate([positions[0], positions[1]], [0, 0]);
    result.into_iter().max().unwrap()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
