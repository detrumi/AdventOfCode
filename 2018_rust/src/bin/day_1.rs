use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let mut n: i32 = 0;
    let mut found: HashSet<i32> = HashSet::new();
    found.insert(0);
    for i in 0.. {
        let file = File::open("input/day_1.txt").unwrap();
        let lines = io::BufReader::new(file).lines();
        for line in lines {
            let current = line.unwrap().parse::<i32>().unwrap();
            n += current;
            if found.contains(&n) {
                println!("Repeating: {}", n);
                return;
            }
            found.insert(n);
        }
        if i == 0 {
            println!("Resulting frequency: {}", n);
        }
    }
}
