const INPUT: &str = include_str!("../../input/day_3.txt");

fn part1() -> usize {
    let mut result = 0;
    for mut line in INPUT.trim().lines().map(|l| l.to_string()) {
        while let Some(index) = line.find("mul(") {
            line = line[index + 4..].to_string();
            let Some(closing) = line.find(")") else {
                break;
            };
            let Some(comma) = line[..closing].find(",") else {
                continue;
            };
            let Ok(left) = line[0..comma].parse::<usize>() else {
                continue;
            };
            let Ok(right) = line[comma + 1..closing].parse::<usize>() else {
                continue;
            };
            result += left * right;
        }
    }
    result
}

fn part2() -> usize {
    let mut result = 0;
    let mut enabled = true;
    for mut line in INPUT.trim().lines().map(|l| l.to_string()) {
        while let Some(index) = line.find("mul(") {
            let enable = line.find("do()");
            let disable = line.find("don't()");
            if let Some(enable) = enable {
                if enable < index {
                    if let Some(disable) = disable {
                        if disable < enable && disable < index {
                            enabled = false;
                            line = line[disable + 7..].to_string();
                            continue;
                        }
                    }

                    enabled = true;
                    line = line[enable + 4..].to_string();
                    continue;
                }
            }
            if let Some(disable) = disable {
                if disable < index {
                    enabled = false;
                    line = line[disable + 7..].to_string();
                    continue;
                }
            }
            line = line[index + 4..].to_string();
            if !enabled {
                continue;
            }
            let Some(closing) = line.find(")") else {
                break;
            };
            let Some(comma) = line[..closing].find(",") else {
                continue;
            };
            let Ok(left) = line[0..comma].parse::<usize>() else {
                continue;
            };
            let Ok(right) = line[comma + 1..closing].parse::<usize>() else {
                continue;
            };
            result += left * right;
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
