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

fn egcd(a: i64, b: i64) -> (i64, i64, i64) {
    if a == 0 {
        (b, 0, 1)
    } else {
        let (g, x, y) = egcd(b % a, a);
        (g, y - (b / a) * x, x)
    }
}

fn mod_inv(x: i64, n: i64) -> i64 {
    let (_, x, _) = egcd(x, n);
    (x % n + n) % n
}

fn chinese_remainder(residues: &[i64], modulii: &[i64]) -> i64 {
    let prod = modulii.iter().product::<i64>();
    let sum: i64 = residues
        .iter()
        .zip(modulii)
        .map(|(&residue, &modulus)| {
            let p = prod / modulus;
            residue * mod_inv(p, modulus) * p
        })
        .sum();

    sum % prod
}

fn part2() -> i64 {
    let ids: Vec<(i64, i64)> = INPUT
        .lines()
        .nth(1)
        .unwrap()
        .split(',')
        .enumerate()
        .filter_map(|(offset, part)| part.parse().ok().map(|id| (offset as i64, id)))
        .collect();
    let residues: Vec<_> = ids.iter().map(|(offset, id)| id - offset).collect();
    let modulii: Vec<_> = ids.iter().map(|(_, id)| *id).collect();
    chinese_remainder(&residues, &modulii)
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
