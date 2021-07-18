use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_1.txt").unwrap();
    let lines = io::BufReader::new(file).lines().map(|l| l.unwrap());

    let mut sum1 = 0;
    let mut sum2 = 0;
    for line in lines {
        let mut fuel = get_mass(line.parse::<i32>().unwrap());
        sum1 += fuel;
        while fuel > 0 {
            sum2 += fuel;
            fuel = get_mass(fuel);
        }
    }
    println!("sum1 = {:?}", sum1);
    println!("sum2 = {:?}", sum2);
}

fn get_mass(n: i32) -> i32 {
    (n / 3) - 2
}
