use std::{collections::HashSet, usize};

use itertools::Itertools;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

const INPUT: &str = include_str!("../../input/day_10.txt");

#[derive(Debug)]
struct Machine {
    indicator: Vec<bool>,
    wirings: Vec<Vec<usize>>,
    reqs: Vec<u16>,
}

fn parse() -> Vec<Machine> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let parts = line.split_ascii_whitespace().collect_vec();
            let indicator = parts[0]
                .chars()
                .filter(|c| ['.', '#'].contains(c))
                .map(|c| c == '#')
                .collect();
            let wirings = parts
                .iter()
                .filter(|part| part.starts_with("("))
                .map(|s| {
                    s[1..s.len() - 1]
                        .split(",")
                        .map(|n| n.parse().unwrap())
                        .collect()
                })
                .collect();
            let reqs = parts.last().unwrap();
            let reqs = reqs[1..reqs.len() - 1]
                .split(",")
                .map(|n| n.parse().unwrap())
                .collect();
            Machine {
                indicator,
                wirings,
                reqs,
            }
        })
        .collect()
}

fn part1() -> usize {
    let mut result = 0;
    'outer: for Machine {
        indicator,
        wirings,
        reqs: _,
    } in parse()
    {
        let mut states = vec![vec![false; indicator.len()]];
        let mut seen = HashSet::new();
        seen.insert(states[0].clone());
        for presses in 1.. {
            let mut new_states = vec![];
            for state in states {
                for wiring in &wirings {
                    let mut state = state.clone();
                    for n in wiring {
                        state[*n] = !state[*n];
                    }
                    if !seen.insert(state.clone()) {
                        continue;
                    }
                    if *state == indicator {
                        result += presses;
                        continue 'outer;
                    }
                    new_states.push(state.clone());
                }
            }
            states = new_states;
        }
    }
    result
}

fn part2() -> usize {
    parse()
        // .iter()
        .into_par_iter()
        .map(|machine| {
            let mut max = usize::MAX;
            let n = solve(&machine, 0, [0; 10], 0, &mut max);
            println!("Done: {n}!");
            n
        })
        .sum()
}

fn solve(
    machine: &Machine,
    index: usize,
    mut state: [u16; 10],
    presses: usize,
    max_presses: &mut usize,
) -> usize {
    let mut best = usize::MAX;
    let wiring = &machine.wirings[index];
    if index == machine.wirings.len() - 1 {
        let i = machine.wirings[index][0];
        let allowed = machine.reqs[i] - state[i];
        let new_presses = presses + allowed as usize;
        if new_presses >= *max_presses {
            return best;
        }
        for &i in wiring {
            state[i] += allowed;
        }
        if state
            .iter()
            .take(machine.reqs.len())
            .zip_eq(&machine.reqs)
            .all(|(a, b)| a == b)
        {
            println!("{}: {:?}", new_presses, state);
            *max_presses = (*max_presses).min(new_presses);
            best = new_presses;
        }
    } else {
        let allowed = wiring
            .iter()
            .map(|&i| machine.reqs[i] - state[i])
            .min()
            .unwrap();
        for n in 0..=allowed {
            let new_presses = presses + n as usize;
            if new_presses >= *max_presses {
                break;
            }
            for &i in wiring {
                state[i] += 1;
            }

            best = best.min(solve(machine, index + 1, state, new_presses, max_presses));
        }
    }

    best
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
