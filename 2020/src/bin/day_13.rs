const INPUT: &str = include_str!("../../input/day_13.txt");

fn part1() -> u32 {
    let input: Vec<_> = INPUT.lines().collect();
    let earliest = input[0].parse::<u32>().unwrap();
    let ids: Vec<u32> = input[1]
        .split(',')
        .filter_map(|part| part.parse().ok())
        .collect();
    for time in earliest.. {
        for id in &ids {
            if time % id == 0 {
                return id * (time - earliest);
            }
        }
    }
    unreachable!()
}

fn part2() -> u128 {
    let input: Vec<_> = INPUT.lines().collect();
    let mut ids: Vec<(u128, u128)> = input[1]
        .split(',')
        .enumerate()
        .filter_map(|(offset, part)| part.parse().ok().map(|id| (offset as u128, id)))
        .collect();
    let line_a = ids[0].1;
    ids = ids.into_iter().skip(1).collect();
    let line_b = ids.iter().find(|(offset, _)| *offset == line_a).unwrap().1;
    ids.sort_by_key(|(_offset, id)| std::cmp::Reverse(*id));
    ids.retain(|(offset, _)| *offset != line_a);

    let prod = line_a * line_b;
    for x in 100_000_000_000_000 / prod.. {
        let n = prod * x - line_a;
        if ids.iter().all(|(offset, line)| (n + offset) % line == 0) {
            return n;
        }
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
