const INPUT: &str = include_str!("../../input/day_4.txt");

fn part1() -> usize {
    let mut result = 0;
    let required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
    let mut current = vec![false; required.len()];

    for line in INPUT.lines() {
        if line.is_empty() {
            if current.iter().all(|b| *b) {
                result += 1;
            }
            current = vec![false; required.len()];
        } else {
            let words: Vec<_> = line.split_ascii_whitespace().collect();
            for word in words {
                let parts: Vec<_> = word.split(':').collect();
                if let Some((i, _)) = required.iter().enumerate().find(|t| t.1 == &parts[0]) {
                    current[i] = true;
                }
            }
        }
    }
    result
}

fn part2() -> usize {
    let mut result = 0;
    let required = ["cid", "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
    let mut current = vec![false; required.len()];

    for line in INPUT.lines() {
        if line.is_empty() {
            if current.iter().enumerate().all(|(i, b)| i == 0 || *b) {
                result += 1;
            }
            current = vec![false; required.len()];
        } else {
            let words: Vec<_> = line.split_ascii_whitespace().collect();
            for word in words {
                let parts: Vec<_> = word.split(':').collect();
                if let Some((i, s)) = required.iter().enumerate().find(|t| t.1 == &parts[0]) {
                    println!("{}", parts[1]);
                    let s = parts[1];
                    current[i] = match parts[0] {
                        "byr" => {
                            s.len() == 4 && (1920..=2002).contains(&s.parse::<usize>().unwrap())
                        }
                        "iyr" => {
                            s.len() == 4 && (2010..=2020).contains(&s.parse::<usize>().unwrap())
                        }
                        "eyr" => {
                            s.len() == 4 && (2020..=2030).contains(&s.parse::<usize>().unwrap())
                        }
                        "hgt" => {
                            if s.ends_with("cm") {
                                (150..=193)
                                    .contains(&s.trim_end_matches("cm").parse::<usize>().unwrap())
                            } else if s.ends_with("in") {
                                (59..=76)
                                    .contains(&s.trim_end_matches("in").parse::<usize>().unwrap())
                            } else {
                                false
                            }
                        }
                        "hcl" => {
                            s.starts_with('#')
                                && s.trim_start_matches('#')
                                    .chars()
                                    .all(|c| ('0'..='9').contains(&c) || ('a'..='f').contains(&c))
                        }
                        "ecl" => ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(&s),
                        "pid" => s.len() == 9 && s.chars().all(|c| c.is_digit(10)),
                        "cid" => true,
                        _ => true,
                    };
                    if current[i] {
                        println!("Valid: {} {}", parts[0], parts[1])
                    }
                }
            }
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
