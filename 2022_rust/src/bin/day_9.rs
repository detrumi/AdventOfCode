use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_9.txt");

fn solve(rope_length: usize) -> Option<usize> {
    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    let mut rope = vec![(0_i32, 0_i32); rope_length];
    for mut parts in INPUT.trim().lines().map(|l| l.split_ascii_whitespace()) {
        let move_ = match parts.next()? {
            "R" => (1, 0),
            "U" => (0, -1),
            "L" => (-1, 0),
            "D" => (0, 1),
            _ => panic!(),
        };
        for _ in 0..parts.next()?.parse().ok()? {
            rope[0] = (rope[0].0 + move_.0, rope[0].1 + move_.1);
            for i in 1..rope.len() {
                let delta = (rope[i - 1].0 - rope[i].0, rope[i - 1].1 - rope[i].1);
                if delta.0.abs() > 1 && delta.1.abs() > 1 {
                    rope[i].0 += delta.0.signum();
                    rope[i].1 += delta.1.signum();
                } else {
                    if delta.0.abs() > 1 {
                        rope[i].0 += delta.0.signum();
                        rope[i].1 = rope[i - 1].1;
                    }
                    if delta.1.abs() > 1 {
                        rope[i].1 += delta.1.signum();
                        rope[i].0 = rope[i - 1].0;
                    }
                }
            }
            visited.insert(*rope.last()?);
        }
    }
    Some(visited.len())
}

fn main() {
    println!("Part 1: {}", solve(2).unwrap());
    println!("Part 2: {}", solve(10).unwrap());
}
