use std::collections::HashMap;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_11.txt");

fn parse() -> Vec<(String, Vec<String>)> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let parts = line.split_ascii_whitespace().collect_vec();
            let device = parts[0][..parts[0].len() - 1].to_string();
            let outputs = parts[1..].iter().map(|s| s.to_string()).collect_vec();
            (device, outputs)
        })
        .collect()
}

fn part1() -> usize {
    let mut result = 0;
    let parsed = parse();
    let mut devices = vec!["you".to_string()];
    while !devices.is_empty() {
        let mut new_devices = vec![];
        for input in devices {
            for (device, outputs) in &parsed {
                if *device == input {
                    for output in outputs {
                        if output == "out" {
                            result += 1;
                        } else {
                            new_devices.push(output.clone());
                        }
                    }
                }
            }
        }
        devices = new_devices.into_iter().collect();
    }
    result
}

#[derive(Default, Debug)]
struct PathCount {
    neither: usize,
    dac: usize,
    fft: usize,
    both: usize,
}

/*
Algo:
- Start from svr
- Only process nodes that have all incoming paths calculated
- Track counts for all combinations
*/

fn part2() -> usize {
    let mut inputs: HashMap<String, Vec<String>> = HashMap::new();
    let mut devices = vec![];
    let parsed = parse();
    for (device, outputs) in parsed {
        if device != "svr" {
            devices.push(device.clone());
        }
        for output in outputs {
            inputs.entry(output).or_default().push(device.clone());
        }
    }

    let mut counts = HashMap::new();
    counts.insert(
        "svr".to_string(),
        PathCount {
            neither: 1,
            dac: 0,
            fft: 0,
            both: 0,
        },
    );

    let mut progressing = true;
    while progressing {
        progressing = false;
        for device_ix in (0..devices.len()).rev() {
            if inputs[&devices[device_ix]]
                .iter()
                .all(|input| counts.contains_key(input))
            {
                progressing = true;
                let device = &devices[device_ix];
                let mut count = PathCount::default();
                for input in &inputs[device] {
                    count.neither += counts[input].neither;
                    count.dac += counts[input].dac;
                    count.fft += counts[input].fft;
                    count.both += counts[input].both;
                }
                if device == "dac" {
                    count.dac += count.neither;
                    count.neither = 0;
                    count.both += count.fft;
                    count.fft = 0;
                } else if device == "fft" {
                    count.fft += count.neither;
                    count.neither = 0;
                    count.both += count.dac;
                    count.dac = 0;
                }

                counts.insert(device.clone(), count);
                devices.remove(device_ix);
            }
        }
    }

    inputs["out"].iter().map(|device| counts[device].both).sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
