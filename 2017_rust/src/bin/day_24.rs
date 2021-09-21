use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_24.txt");

fn parse() -> Vec<(usize, usize)> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            line.split("/")
                .map(|s| s.parse::<usize>().unwrap())
                .collect_tuple()
                .unwrap()
        })
        .collect()
}

fn part1(port: usize, components: Vec<(usize, usize)>) -> usize {
    let mut best = 0;
    for (ix, (p1, p2)) in components.iter().enumerate() {
        if *p1 == port || *p2 == port {
            let mut leftovers = components.clone();
            leftovers.swap_remove(ix);
            let other_port = if *p1 == port { *p2 } else { *p1 };
            let score = p1 + p2 + part1(other_port, leftovers);
            best = best.max(score);
        }
    }
    best
}

fn part2(port: usize, length: usize, components: Vec<(usize, usize)>) -> (usize, usize) {
    let (mut best, mut best_length) = (0, length);
    for (ix, (p1, p2)) in components.iter().enumerate() {
        if *p1 == port || *p2 == port {
            let mut leftovers = components.clone();
            leftovers.swap_remove(ix);
            let other_port = if *p1 == port { *p2 } else { *p1 };
            let (mut score, new_length) = part2(other_port, length + 1, leftovers);
            score += p1 + p2;
            if new_length > best_length || new_length == best_length && score > best {
                best = score;
                best_length = new_length;
            }
        }
    }
    (best, best_length)
}

fn main() {
    let components = parse();
    println!("Part 1: {}", part1(0, components.clone()));
    println!("Part 2: {}", part2(0, 0, components).0);
}
