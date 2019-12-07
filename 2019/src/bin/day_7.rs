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

    let mut max_val = std::i32::MIN;
    let mut sequence = vec![0, 1, 2, 3, 4];
    loop {
        let mut val = 0;
        for i in 0..5 {
            val = calculate(codes.clone(), &vec![sequence[i], val])
        }
        if val > max_val {
            max_val = val;
        }
        if !sequence.next_permutation() {
            break;
        }
    }
    eprintln!("Part 1 = {:?}", max_val);

    let mut max_val = std::i32::MIN;
    let mut sequence = vec![5, 6, 7, 8, 9];
    loop {
        let mut amps = vec![];
        for i in 0..5 {
            amps.push(Amp::new(codes.clone(), sequence[i]));
        }
        amps[0].inputs.push(0);

        let mut old_val = 0;
        let mut searching = true;
        while searching {
            for i in 0..5 {
                if let Some(val) = amps[i].calculate() {
                    old_val = val;
                    amps[(i + 1) % 5].inputs.push(val);
                } else {
                    if old_val > max_val {
                        max_val = old_val;
                    }
                    searching = false;
                    break;
                }
            }
        }

        if !sequence.next_permutation() {
            break;
        }
    }
    eprintln!("Part 2 = {:?}", max_val);
}

fn calculate(mut codes: Vec<i32>, phases: &Vec<i32>) -> i32 {
    let mut last_output = 0;
    let mut phase_index = 0;

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
                if phase_index >= phases.len() {
                    panic!("No more inputs!")
                }
                let target = codes[i + 1];
                codes[target as usize] = phases[phase_index];
                phase_index += 1;
                i += 2;
            }
            4 => {
                last_output = codes[codes[i + 1] as usize];
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
            99 => return last_output,
            n => panic!(format!("{}", n)),
        }
    }
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

    fn calculate(&mut self) -> Option<i32> {
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
