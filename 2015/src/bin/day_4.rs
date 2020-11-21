const INPUT: &str = include_str!("../../input/day_4.txt");

fn find_prefix(prefix: &str) -> i32 {
    for n in 1.. {
        let hash = md5::compute(format!("{}{}", INPUT, n));
        if format!("{:?}", hash).starts_with(prefix) {
            return n;
        }
    }
    unreachable!()
}

fn part1() -> i32 {
    find_prefix(&"0".repeat(5))
}

fn part2() -> i32 {
    find_prefix(&"0".repeat(6))
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
