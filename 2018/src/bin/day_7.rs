use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_7.txt").unwrap();
    let pairs: Vec<(char, char)> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .map(|line| {
            let parts = line.split(" ").collect::<Vec<_>>();
            (
                parts[1].chars().next().unwrap(),
                parts[7].chars().next().unwrap(),
            )
        })
        .collect();

    let all_steps: HashSet<char> = pairs
        .iter()
        .flat_map(|(a, b)| vec![a.clone(), b.clone()])
        .collect();

    part_1(pairs.clone(), all_steps.clone());
    part_2(pairs, all_steps);
}

fn part_1(mut pairs: Vec<(char, char)>, mut all_steps: HashSet<char>) {
    let mut result = String::new();
    while !all_steps.is_empty() {
        let mut open: Vec<char> = all_steps
            .iter()
            .filter(|&s| pairs.iter().filter(|(_, b)| b == s).count() == 0)
            .cloned()
            .collect();
        open.sort();
        let choice = open[0];

        all_steps.remove(&choice);
        result.push(choice);
        pairs.retain(|(a, _b)| a != &choice);
    }
    println!("Part 1: {}", result);
}

const STEP_BASE_COST: i32 = 60;
const WORKERS: usize = 5;

fn part_2(mut pairs: Vec<(char, char)>, mut all_steps: HashSet<char>) {
    let mut remaining_work: Vec<(i32, Option<char>)> = vec![(0, None); WORKERS];
    let mut result = String::new();
    let mut seconds = -1;
    while !all_steps.is_empty() || remaining_work.iter().any(|(_, b)| b.is_some()) {
        remaining_work.sort_by_key(|x| x.1);
        for (time_left, job) in &mut remaining_work {
            if let Some(job_id) = job {
                *time_left -= 1;
                if *time_left == 0 {
                    pairs.retain(|p| p.0 != *job_id);
                    result.push(*job_id);
                    *job = None;
                }
            }
        }

        for worker in remaining_work.iter_mut().filter(|(_, j)| j.is_none()) {
            let mut open: Vec<char> = all_steps
                .iter()
                .filter(|&s| pairs.iter().filter(|p| p.1 == *s).count() == 0)
                .cloned()
                .collect();
            open.sort();
            if let Some(choice) = open.get(0) {
                *worker = (
                    STEP_BASE_COST + *choice as i32 - 'A' as i32 + 1,
                    Some(*choice),
                );
                all_steps.remove(&choice);
            }
        }
        seconds += 1;
    }
    println!("Part 2: {}", seconds);
}
