use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

const WIDTH: usize = 25;
const HEIGHT: usize = 6;

fn main() {
    let file = File::open("input/day_8.txt").unwrap();
    let numbers: Vec<_> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|n| n.to_digit(10).unwrap())
                .collect::<Vec<_>>()
        })
        .next()
        .unwrap();

    let num_layers = numbers.len() / (WIDTH * HEIGHT);

    let mut min_zero = std::i32::MAX;
    let mut part_1 = 0;
    let mut colors: Vec<i32> = vec![2; WIDTH * HEIGHT];
    for layer in 0..num_layers {
        let mut counts: HashMap<i32, i32> = HashMap::new();
        for y in 0..HEIGHT {
            for x in 0..WIDTH {
                let index = layer * (WIDTH * HEIGHT) + y * WIDTH + x;
                *counts.entry(numbers[index] as i32).or_default() += 1;
                if colors[y * WIDTH + x] == 2 {
                    colors[y * WIDTH + x] = numbers[index] as i32;
                }
            }
        }
        let num_zero = *counts.entry(0).or_default();
        if num_zero < min_zero {
            min_zero = num_zero;
            part_1 = counts[&1] * counts[&2];
        }
    }
    println!("Part 1 = {:?}", part_1);

    println!("Part 2 =");
    for y in 0..HEIGHT {
        for x in 0..WIDTH {
            print!("{}", if colors[y * WIDTH + x] == 1 { "#" } else { " " });
        }
        println!();
    }
}
