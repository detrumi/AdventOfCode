const INPUT: &str = include_str!("../../input/day_5.txt");

fn part1() -> usize {
    let mut best = 0;
    for line in INPUT.lines() {
        let mut r = 0..128;
        let mut c = 0..8;
        line.chars().for_each(|ch| match ch {
            'F' => r = r.start..r.end - (r.end - r.start + 1) / 2,
            'B' => r = r.start + (r.end - r.start + 1) / 2..r.end,
            'L' => c = c.start..c.end - (c.end - c.start + 1) / 2,
            'R' => c = c.start + (c.end - c.start + 1) / 2..c.end,
            _ => panic!(),
        });
        best = best.max(8 * r.start + c.end - 1);
    }
    best
}

fn part2() -> usize {
    let mut found = vec![];
    for line in INPUT.lines() {
        let mut r = 0..128;
        let mut c = 0..8;
        line.chars().for_each(|ch| match ch {
            'F' => r = r.start..r.end - (r.end - r.start + 1) / 2,
            'B' => r = r.start + (r.end - r.start + 1) / 2..r.end,
            'L' => c = c.start..c.end - (c.end - c.start + 1) / 2,
            'R' => c = c.start + (c.end - c.start + 1) / 2..c.end,
            _ => panic!(),
        });
        found.push(8 * r.start + c.end - 1);
    }
    found.sort();
    found
        .iter()
        .zip(found[0]..)
        .find(|(&a, b)| a != *b)
        .unwrap()
        .1
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
