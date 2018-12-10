use std::fs::File;
use std::io::{self, BufRead};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Default)]
struct Pos {
    x: i32,
    y: i32,
}

impl Pos {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

fn main() {
    let file = File::open("input/day_10.txt").unwrap();
    let lines = io::BufReader::new(file).lines().map(|l| l.unwrap());

    let mut data: Vec<(Pos, Pos)> = vec![];
    for line in lines {
        let parts = line
            .split('<')
            .skip(1)
            .flat_map(|p| {
                p.split('>')
                    .next()
                    .unwrap()
                    .split(", ")
                    .map(|s| s.trim().parse::<i32>().unwrap())
            })
            .collect::<Vec<_>>();

        data.push((Pos::new(parts[0], parts[1]), Pos::new(parts[2], parts[3])));
    }

    calculate(&mut data);
}

fn calculate(data: &mut Vec<(Pos, Pos)>) -> Option<()> {
    let stdin = io::stdin();
    let mut t = 0;
    loop {
        let x_min = data.iter().map(|t| t.0.x).min()?;
        let y_min = data.iter().map(|t| t.0.y).min()?;
        let x_max = data.iter().map(|t| t.0.x).max()?;
        let y_max = data.iter().map(|t| t.0.y).max()?;
        println!("({}, {}) to ({}, {})", x_min, y_min, x_max, y_max);

        if x_max - x_min < 200 {
            println!("t={}", t);
            for y in y_min..=y_max {
                for x in x_min..=x_max {
                    if data.iter().any(|t| t.0 == Pos { x, y }) {
                        print!("#");
                    } else {
                        print!(".");
                    }
                }
                println!();
            }
            stdin.lock().lines().next().unwrap().unwrap();
        }

        for (pos, vel) in data.iter_mut() {
            pos.x += vel.x;
            pos.y += vel.y;
        }
        t += 1;
    }
}
