use lazy_static::lazy_static;
use permutohedron::LexicalPermutation;

const INPUT: &str = include_str!("../../input/day_8.txt");

lazy_static! {
    static ref SEGMENTS: [Vec<usize>; 10] = [
        vec![0, 1, 2, 4, 5, 6],
        vec![2, 5],
        vec![0, 2, 3, 4, 6],
        vec![0, 2, 3, 5, 6],
        vec![1, 2, 3, 5],
        vec![0, 1, 3, 5, 6],
        vec![0, 1, 3, 4, 5, 6],
        vec![0, 2, 5],
        vec![0, 1, 2, 3, 4, 5, 6],
        vec![0, 1, 2, 3, 5, 6],
    ];
}

fn parse() -> Vec<(Vec<String>, Vec<String>)> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let (left, right) = line.split_once(" | ").unwrap();
            (
                left.split_ascii_whitespace()
                    .map(|s| s.to_string())
                    .collect(),
                right
                    .split_ascii_whitespace()
                    .map(|s| s.to_string())
                    .collect(),
            )
        })
        .collect()
}

fn decode(input: &str, ordering: &[usize]) -> Option<usize> {
    let mut decoded: Vec<usize> = input
        .chars()
        .map(|c| c as usize - 'a' as usize)
        .map(|n| ordering[n])
        .collect();
    decoded.sort();
    SEGMENTS.iter().position(|segment| *segment == decoded)
}

fn part1() -> usize {
    parse()
        .iter()
        .map(|(_input, output)| {
            output
                .iter()
                .filter(|o| [2, 3, 4, 7].contains(&o.len()))
                .count()
        })
        .sum()
}

fn part2() -> usize {
    parse()
        .iter()
        .map(|(input, output)| {
            let mut ordering: Vec<usize> = (0..7).collect();
            while !input.iter().all(|s| decode(s, &ordering).is_some()) {
                ordering.next_permutation();
            }

            output
                .iter()
                .map(|o| decode(o, &ordering).unwrap().to_string())
                .collect::<String>()
                .parse::<usize>()
                .unwrap()
        })
        .sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
