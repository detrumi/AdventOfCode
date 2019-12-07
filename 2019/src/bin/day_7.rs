use permutohedron::LexicalPermutation;
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_7.txt").unwrap();
    let codes: Vec<i32> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            l.unwrap()
                .split(",")
                .map(|n| n.parse::<i32>().unwrap())
                .collect::<Vec<_>>()
        })
        .next()
        .unwrap();

    part_1(&codes);
    part_2(&codes);
}

fn part_1(codes: &Vec<i32>) {
    let mut max_val = std::i32::MIN;
    let mut sequence: Vec<i32> = (0..=4).collect();
    loop {
        let mut val = 0;
        for i in 0..5 {
            val = Amp::new(codes.clone(), sequence[i]).calculate(val).unwrap();
        }
        max_val = max_val.max(val);
        if !sequence.next_permutation() {
            break;
        }
    }
    eprintln!("Part 1 = {:?}", max_val);
}

fn part_2(codes: &Vec<i32>) {
    let mut max_val = std::i32::MIN;
    let mut sequence: Vec<i32> = (5..=9).collect();
    loop {
        let mut amps: Vec<Amp> = sequence
            .iter()
            .map(|n| Amp::new(codes.clone(), *n))
            .collect();

        let mut val = 0;
        for i in 0.. {
            match amps[i % 5].calculate(val) {
                Some(new_val) => val = new_val,
                None => break,
            }
        }
        max_val = max_val.max(val);
        if !sequence.next_permutation() {
            break;
        }
    }
    eprintln!("Part 2 = {:?}", max_val);
}

#[derive(Clone)]
struct Amp {
    codes: Vec<i32>,
    index: usize,
    inputs: Vec<i32>,
}

impl Amp {
    fn new(codes: Vec<i32>, phase: i32) -> Self {
        Self {
            codes,
            index: 0,
            inputs: vec![phase],
        }
    }

    fn calculate(&mut self, new_input: i32) -> Option<i32> {
        self.inputs.push(new_input);
        loop {
            let mut n = self.codes[self.index] / 100;
            let mut inputs = vec![];
            while n > 0 {
                inputs.push(n % 10);
                n /= 10;
            }
            while inputs.len() < 2 {
                inputs.push(0);
            }

            let param = |index| {
                let target = self.codes[self.index + index + 1];
                if inputs[index] == 0 {
                    self.codes[target as usize]
                } else {
                    target
                }
            };
            match self.codes[self.index] % 100 {
                1 => {
                    let target = self.codes[self.index + 3];
                    self.codes[target as usize] = param(0) + param(1);
                    self.index += 4;
                }
                2 => {
                    let target = self.codes[self.index + 3];
                    self.codes[target as usize] = param(0) * param(1);
                    self.index += 4;
                }
                3 => {
                    let target = self.codes[self.index + 1];
                    self.codes[target as usize] = self.inputs[0];
                    self.inputs.remove(0);
                    self.index += 2;
                }
                4 => {
                    let output = self.codes[self.codes[self.index + 1] as usize];
                    self.index += 2;
                    return Some(output);
                }
                5 => {
                    if param(0) != 0 {
                        self.index = param(1) as usize;
                    } else {
                        self.index += 3;
                    }
                }
                6 => {
                    if param(0) == 0 {
                        self.index = param(1) as usize;
                    } else {
                        self.index += 3;
                    }
                }
                7 => {
                    let target = self.codes[self.index + 3];
                    self.codes[target as usize] = if param(0) < param(1) { 1 } else { 0 };
                    self.index += 4;
                }
                8 => {
                    let target = self.codes[self.index + 3];
                    self.codes[target as usize] = if param(0) == param(1) { 1 } else { 0 };
                    self.index += 4;
                }
                99 => return None,
                n => panic!(format!("{}", n)),
            }
        }
    }
}
