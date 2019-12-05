use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    eprintln!("Part 1 = {:?}", calculate(1));
    eprintln!("Part 2 = {:?}", calculate(5));
}

fn calculate(input_id: i32) {
    let file = File::open("input/day_5.txt").unwrap();
    let mut codes: Vec<i32> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            l.unwrap()
                .split(",")
                .map(|s| s.parse::<i32>().unwrap())
                .collect()
        })
        .next()
        .unwrap();

    let mut i: usize = 0;
    loop {
        let mut n = codes[i] / 100;
        let mut inputs = vec![];
        while n > 0 {
            inputs.push(n % 10);
            n /= 10;
        }
        while inputs.len() < 2 {
            inputs.push(0);
        }

        let param = |index| {
            let target = codes[i + index + 1];
            if inputs[index] == 0 {
                codes[target as usize]
            } else {
                target
            }
        };
        match codes[i] % 100 {
            1 => {
                let target = codes[i + 3];
                codes[target as usize] = param(0) + param(1);
                i += 4;
            }
            2 => {
                let target = codes[i + 3];
                codes[target as usize] = param(0) * param(1);
                i += 4;
            }
            3 => {
                let target = codes[i + 1];
                codes[target as usize] = input_id;
                i += 2;
            }
            4 => {
                println!("Output = {}", codes[codes[i + 1] as usize]);
                i += 2;
            }
            5 => {
                if param(0) != 0 {
                    i = param(1) as usize;
                } else {
                    i += 3;
                }
            }
            6 => {
                if param(0) == 0 {
                    i = param(1) as usize;
                } else {
                    i += 3;
                }
            }
            7 => {
                let target = codes[i + 3];
                codes[target as usize] = if param(0) < param(1) { 1 } else { 0 };
                i += 4;
            }
            8 => {
                let target = codes[i + 3];
                codes[target as usize] = if param(0) == param(1) { 1 } else { 0 };
                i += 4;
            }
            99 => return,
            n => panic!(format!("{}", n)),
        }
    }
}
