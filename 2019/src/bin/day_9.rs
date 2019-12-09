use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    eprintln!("Part 1 = {:?}", calculate(1));
    eprintln!("Part 2 = {:?}", calculate(2));
}

fn calculate(input_id: i64) {
    let file = File::open("input/day_9.txt").unwrap();
    let mut codes: HashMap<usize, i64> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            l.unwrap()
                .split(",")
                .map(|s| s.parse::<i64>().unwrap())
                .enumerate()
                .collect()
        })
        .next()
        .unwrap();

    let mut i: usize = 0;
    let mut relative_base: i64 = 0;
    loop {
        let mut n = *codes.entry(i).or_default() / 100;
        let mut inputs = vec![];
        while n > 0 {
            inputs.push(n % 10);
            n /= 10;
        }
        while inputs.len() < 3 {
            inputs.push(0);
        }

        let code = *codes.entry(i).or_default() % 100;
        match code {
            1 => {
                let value = param(0, i, relative_base, &inputs, &mut codes)
                    + param(1, i, relative_base, &inputs, &mut codes);
                set_param(2, i, relative_base, &inputs, &mut codes, value);
                i += 4;
            }
            2 => {
                let value = param(0, i, relative_base, &inputs, &mut codes)
                    * param(1, i, relative_base, &inputs, &mut codes);
                set_param(2, i, relative_base, &inputs, &mut codes, value);
                i += 4;
            }
            3 => {
                set_param(0, i, relative_base, &inputs, &mut codes, input_id);
                i += 2;
            }
            4 => {
                let value = param(0, i, relative_base, &inputs, &mut codes);
                println!("Output = {}", value);
                i += 2;
            }
            5 => {
                if param(0, i, relative_base, &inputs, &mut codes) != 0 {
                    i = param(1, i, relative_base, &inputs, &mut codes) as usize;
                } else {
                    i += 3;
                }
            }
            6 => {
                if param(0, i, relative_base, &inputs, &mut codes) == 0 {
                    i = param(1, i, relative_base, &inputs, &mut codes) as usize;
                } else {
                    i += 3;
                }
            }
            7 => {
                let value = if param(0, i, relative_base, &inputs, &mut codes)
                    < param(1, i, relative_base, &inputs, &mut codes)
                {
                    1
                } else {
                    0
                };
                set_param(2, i, relative_base, &inputs, &mut codes, value);
                i += 4;
            }
            8 => {
                let value = if param(0, i, relative_base, &inputs, &mut codes)
                    == param(1, i, relative_base, &inputs, &mut codes)
                {
                    1
                } else {
                    0
                };
                set_param(2, i, relative_base, &inputs, &mut codes, value);
                i += 4;
            }
            9 => {
                relative_base += param(0, i, relative_base, &inputs, &mut codes);
                i += 2;
            }
            99 => return,
            n => panic!(format!("{}", n)),
        }
    }
}

fn param(
    index: usize,
    i: usize,
    relative_base: i64,
    inputs: &Vec<i64>,
    codes: &mut HashMap<usize, i64>,
) -> i64 {
    let target: i64 = *codes.entry(i + index + 1).or_default();
    if inputs[index] == 0 {
        *codes.entry(target as usize).or_default()
    } else if inputs[index] == 1 {
        target
    } else {
        let val = (i + index + 1) as i64;
        let target: i64 = *codes.entry(val as usize).or_default() + relative_base;
        *codes.entry(target as usize).or_default()
    }
}

fn set_param(
    index: usize,
    i: usize,
    relative_base: i64,
    inputs: &Vec<i64>,
    codes: &mut HashMap<usize, i64>,
    value: i64,
) {
    let target: i64 = *codes.entry(i + index + 1).or_default();
    if inputs[index] == 0 {
        *codes.entry(target as usize).or_default() = value;
    } else if inputs[index] == 1 {
        panic!();
    } else {
        let val = (i + index + 1) as i64;
        let target: i64 = *codes.entry(val as usize).or_default() + relative_base;
        *codes.entry(target as usize).or_default() = value;
    }
}
