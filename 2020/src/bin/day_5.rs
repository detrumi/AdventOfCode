const INPUT: &str = include_str!("../../input/day_5.txt");

fn part1() -> usize {
    let mut best = 0;
    for line in INPUT.lines() {
        let mut r = (0, 127);
        let mut c = (0, 7);
        for curr in line.chars() {
            match curr {
                'F' => r = (r.0, r.1 - (r.1 - r.0 + 1) / 2),
                'B' => r = (r.0 + (r.1 - r.0 + 1) / 2, r.1),
                'L' => c = (c.0, c.1 - (c.1 - c.0 + 1) / 2),
                'R' => c = (c.0 + (c.1 - c.0 + 1) / 2, c.1),
                _ => panic!(),
            }
        }
        best = best.max(8 * r.0 + c.1);
    }
    best
}

fn part2() -> usize {
    let mut best = 0;
    let mut found = vec![];
    for line in INPUT.lines() {
        let mut r = (0, 127);
        let mut c = (0, 7);
        for curr in line.chars() {
            match curr {
                'F' => r = (r.0, r.1 - (r.1 - r.0 + 1) / 2),
                'B' => r = (r.0 + (r.1 - r.0 + 1) / 2, r.1),
                'L' => c = (c.0, c.1 - (c.1 - c.0 + 1) / 2),
                'R' => c = (c.0 + (c.1 - c.0 + 1) / 2, c.1),
                _ => panic!(),
            }
        }
        found.push(8 * r.0 + c.1);
        best = best.max(8 * r.0 + c.1);
    }
    found.sort();
    let mut curr = found[0];
    for i in found {
        if i - curr > 1 {
            return i - 1;
        }
        curr = i;
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
