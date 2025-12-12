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

// fn part2() -> usize {
//     // let mut result = 0;
//     // let parsed = parse().into_iter().collect::<FxHashMap<_, _>>();
//     // let mut devices = vec!["svr".to_string()];
//     // while !devices.is_empty() {
//     //     println!("{}", devices.len());
//     //     let mut new_devices = vec![];
//     //     for input in devices {
//     //         let outputs = &parsed[&input];
//     //         for output in outputs {
//     //             if output == "out" {
//     //                 result += 1;
//     //             } else {
//     //                 new_devices.push(output.clone());
//     //             }
//     //         }
//     //     }
//     //     devices = new_devices.into_iter().collect();
//     // }
//     // result

//     let parsed = parse().into_iter().collect::<FxHashMap<_, _>>();
//     solve(&parsed, &"svr".to_string())
// }

// fn solve(parsed: &FxHashMap<String, Vec<String>>, input: &String) -> usize {
//     let mut result = 0;
//     let outputs = &parsed[input];
//     for output in outputs {
//         if output == "out" {
//             result += 1;
//         } else {
//             result += solve(parsed, &output);
//             eprintln!("result = {:?}", result);
//         }
//     }
//     result
// }

fn part2() -> usize {
    let mut result = 0;
    let parsed = parse();
    let mut devices = vec![("out".to_string(), false, false)];
    // let paths = FxHashMap::new();
    while !devices.is_empty() {
        eprintln!("devices.len() = {:?}", devices.len());
        let mut new_devices = vec![];
        for (target, dac, fft) in devices {
            if target == "svr" && dac && fft {
                result += 1;
            }
            for (device, outputs) in &parsed {
                if outputs.contains(&target) {
                    new_devices.push((
                        device.clone(),
                        dac || device == "dac",
                        fft || device == "fft",
                    ));
                }
            }
        }
        devices = new_devices;
    }
    result
}

fn main() {
    println!("Part 1: {}", part1()); // 37, 21 wrong
    println!("Part 2: {}", part2());
}
