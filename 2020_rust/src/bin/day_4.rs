const INPUT: &str = include_str!("../../input/day_4.txt");
const REQUIRED: [&str; 7] = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

fn parse<'a>() -> Vec<Vec<(&'a str, &'a str)>> {
    let mut result = vec![vec![]];
    for line in INPUT.lines() {
        if line.is_empty() {
            result.push(vec![]);
        } else {
            for word in line.split_ascii_whitespace() {
                let parts: Vec<_> = word.split(':').collect();
                if REQUIRED.contains(&parts[0]) {
                    result.last_mut().unwrap().push((parts[0], parts[1]));
                }
            }
        }
    }
    result
}

fn solve<'a, F>(check: F) -> usize
where
    F: Fn(&'a str, &'a str) -> bool,
{
    parse()
        .iter()
        .filter(|&passport| {
            passport.iter().filter(|(field, s)| check(field, s)).count() == REQUIRED.len()
        })
        .count()
}

fn check_field<'a>(field: &'a str, s: &'a str) -> bool {
    match field {
        "byr" => matches!(s.parse().unwrap(), 1920..=2002),
        "iyr" => matches!(s.parse().unwrap(), 2010..=2020),
        "eyr" => matches!(s.parse().unwrap(), 2020..=2030),
        "hgt" if s.ends_with("cm") => {
            matches!(s.trim_end_matches("cm").parse().unwrap(), 150..=193)
        }
        "hgt" if s.ends_with("in") => matches!(s.trim_end_matches("in").parse().unwrap(), 59..=76),
        "hcl" => s.starts_with('#') && s.trim_start_matches('#').chars().all(|c| c.is_digit(16)),
        "ecl" => ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(&s),
        "pid" => s.len() == 9 && s.chars().all(|c| c.is_digit(10)),
        _ => false,
    }
}

fn main() {
    println!("Part 1: {}", solve(|_, _| true));
    println!("Part 2: {}", solve(check_field));
}
