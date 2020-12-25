const INPUT: &str = include_str!("../../input/day_25.txt");

fn solve(numbers: Vec<u64>) -> u64 {
    let mut value = 1;
    let mut key = 1;
    loop {
        key = (key * numbers[0]) % 20201227;
        value = (value * 7) % 20201227;

        if value == numbers[1] {
            break key;
        }
    }
}

fn main() {
    let numbers = INPUT.lines().map(|l| l.parse().unwrap()).collect();
    println!("Solution: {}", solve(numbers));
}
