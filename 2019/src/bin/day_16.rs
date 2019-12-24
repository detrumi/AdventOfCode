use std::char;
use std::cmp;
use std::fs::File;
use std::io::{self, BufRead};
use std::ops::Range;

fn main() {
    let file = File::open("input/day_16.txt").unwrap();
    let inputs: Vec<_> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|n| n.to_digit(10).unwrap() as i32)
                .collect::<Vec<_>>()
        })
        .next()
        .unwrap();

    println!("Part 1: {}", part1(inputs.clone()));
    println!("Part 2: {}", part2(inputs.clone()));
}

fn part1(mut inputs: Vec<i32>) -> String {
    let mut output = vec![0; inputs.len()];
    for _phase in 0..100 {
        for pattern_length in 1..=inputs.len() {
            output[pattern_length - 1] = get_sum(&inputs, pattern_length);
        }
        inputs = output.clone();
    }
    inputs[0..8]
        .iter()
        .map(|n| char::from_digit(*n as u32, 10).unwrap())
        .collect::<String>()
}

fn get_sum(inputs: &[i32], pattern_length: usize) -> i32 {
    let mut sum: i32 = 0;
    for repetition in 0..=inputs.len() / (4 * pattern_length) {
        let base = 4 * repetition * pattern_length - 1;
        if base + pattern_length > inputs.len() {
            break;
        }
        sum += inputs[base + pattern_length..cmp::min(base + 2 * pattern_length, inputs.len())]
            .iter()
            .sum::<i32>();

        if base + 3 * pattern_length > inputs.len() {
            break;
        }
        sum -= inputs[base + 3 * pattern_length..cmp::min(base + 4 * pattern_length, inputs.len())]
            .iter()
            .sum::<i32>();
    }
    (sum % 10).abs() as i32
}

fn part2(mut inputs: Vec<i32>) -> String {
    let message_offset: usize = inputs
        .iter()
        .take(7)
        .map(|n| char::from_digit(*n as u32, 10).unwrap())
        .collect::<String>()
        .parse::<usize>()
        .unwrap();

    let repeats = 10_000;
    let mut new_inputs = vec![];
    new_inputs.reserve(repeats * inputs.len());
    for _i in 0..repeats {
        new_inputs.extend(inputs.clone());
    }
    inputs = new_inputs;

    const PHASES: usize = 100;
    let mut required: Vec<Vec<Range<usize>>> = vec![vec![message_offset..message_offset + 7]];
    for _phase in 0..PHASES {
        let mut new_required: Vec<Range<usize>> = vec![];
        for Range { start, end } in &required[required.len() - 1] {
            for i in *start..*end {
                let pattern_length = i - 1;
                for repetition in 0..=inputs.len() / (4 * pattern_length) {
                    let base = 4 * repetition * pattern_length - 1;
                    if base + pattern_length > inputs.len() {
                        break;
                    }
                    new_required.push(
                        base + pattern_length..cmp::min(base + 2 * pattern_length, inputs.len()),
                    );

                    if base + 3 * pattern_length > inputs.len() {
                        break;
                    }
                    new_required.push(
                        base + 3 * pattern_length
                            ..cmp::min(base + 4 * pattern_length, inputs.len()),
                    );
                }
            }
        }
        let mut deduplicated: Vec<Range<usize>> = vec![];
        'outer: for range in &new_required {
            for i in 0..deduplicated.len() {
                if deduplicated[i].contains(&range.start) || range.contains(&deduplicated[i].start)
                {
                    deduplicated[i] =
                        range.start.min(deduplicated[i].start)..range.end.max(deduplicated[i].end);
                    continue 'outer;
                }
            }
            deduplicated.push(range.clone());
        }

        required.push(deduplicated);
    }

    required.reverse();
    for phase in 0..PHASES {
        println!("phase = {}", phase);
        let mut new_inputs = inputs.clone();
        for Range { start, end } in &required[phase] {
            for i in *start..*end {
                new_inputs[i] = get_sum(&inputs, i + 1);
            }
        }
        inputs = new_inputs;
    }
    inputs[message_offset..=message_offset + 7]
        .iter()
        .map(|n| char::from_digit(*n as u32, 10).unwrap())
        .collect::<String>()
}
