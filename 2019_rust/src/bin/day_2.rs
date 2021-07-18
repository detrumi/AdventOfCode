use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    eprintln!("Part 1 = {:?}", calculate(12, 2));

    for a in 0..100 {
        for b in 0..100 {
            let val = calculate(a, b);
            if val == 19_690_720 {
                eprintln!("100 * a + b = {:?}", 100 * a + b);
                return;
            }
        }
    }
}

fn calculate(input_a: usize, input_b: usize) -> usize {
    let file = File::open("input/day_2.txt").unwrap();
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .collect();

    let mut codes: Vec<usize> = lines[0]
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect();
    codes[1] = input_a;
    codes[2] = input_b;

    let mut i: usize = 0;
    loop {
        match codes[i] {
            1 | 2 => {
                let (a, b) = (codes[codes[i + 1]], codes[codes[i + 2]]);
                let target = codes[i + 3];
                codes[target] = if codes[i] == 1 { a + b } else { a * b };
                i += 4;
            }
            99 => break,
            _ => panic!(),
        }
    }
    codes[0]
}
