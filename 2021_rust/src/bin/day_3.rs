const INPUT: &str = include_str!("../../input/day_3.txt");

fn dec(v: &[char]) -> usize {
    let mut result = 0;
    for i in 0..v.len() {
        if v[i] == '1' {
            result += 2_usize.pow((v.len() - i - 1) as u32);
        }
    }
    result
}

fn part1() -> usize {
    let lines: Vec<_> = INPUT.trim().lines().collect();
    let mut counts = vec![0; lines[0].len()];
    for i in 0..lines.len() {
        let chars: Vec<_> = lines[i].chars().collect();
        for j in 0..lines[0].len() {
            if chars[j] == '1' {
                counts[j] += 1;
            }
        }
    }
    let mut gamma = 0;
    for i in 0..counts.len() {
        if counts[i] >= lines.len() / 2 {
            gamma += 1 << (counts.len() - i - 1);
        }
    }
    let epsilon = gamma ^ ((1 << counts.len()) - 1);
    gamma * epsilon
}

fn part2() -> usize {
    let lines: Vec<_> = INPUT
        .trim()
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect();
    let mut oxygen = lines.clone();
    let mut scrubber = lines.clone();
    for n in 0..lines[0].len() {
        let mut counts_o = vec![0; oxygen[0].len()];
        for i in 0..oxygen.len() {
            for j in 0..oxygen[0].len() {
                if oxygen[i][j] == '1' {
                    counts_o[j] += 1;
                }
            }
        }

        let mut counts_s = vec![0; scrubber[0].len()];
        for i in 0..scrubber.len() {
            for j in 0..scrubber[0].len() {
                if scrubber[i][j] == '1' {
                    counts_s[j] += 1;
                }
            }
        }

        if oxygen.len() > 1 {
            let old = oxygen.clone();
            oxygen.retain(|v| {
                if 2 * counts_o[n] < old.len() {
                    return v[n] == '0';
                };
                return v[n] == '1';
            });
        }
        if scrubber.len() > 1 {
            let old = scrubber.clone();
            scrubber.retain(|v| {
                if 2 * counts_s[n] < old.len() {
                    return v[n] == '1';
                };
                return v[n] == '0';
            });
        }
    }
    dec(&oxygen[0]) * dec(&scrubber[0])
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
