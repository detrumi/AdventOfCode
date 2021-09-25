const INPUT: u32 = 36_000_000;

fn part1() -> usize {
    let max = 1_000_000;
    let mut sieve = vec![0_u32; max];
    for n in 1..=max {
        for i in 1..=max / n {
            sieve[i * n - 1] += n as u32;
        }
    }
    sieve.iter().position(|&n| n >= INPUT / 10).unwrap() + 1
}

fn part2() -> usize {
    let max = 1_000_000;
    let mut sieve = vec![0_u32; max];
    for n in 1..=max {
        for i in 1..=50.min(max / n) {
            sieve[i * n - 1] += n as u32;
        }
    }
    sieve.iter().position(|&n| n >= INPUT / 11).unwrap() + 1
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
