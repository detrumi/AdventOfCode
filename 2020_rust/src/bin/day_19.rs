use std::collections::HashMap;

const INPUT: &str = include_str!("../../input/day_19.txt");

#[derive(Debug)]
enum Rule {
    Char(char),
    Others(Vec<Vec<usize>>),
}

impl Rule {
    pub fn parse(s: &str) -> Self {
        if s.starts_with('"') {
            Rule::Char(s.trim_matches('"').chars().next().unwrap())
        } else {
            Rule::Others(
                s.split(" | ")
                    .map(|p| p.split(' ').map(|s| s.parse().unwrap()).collect())
                    .collect(),
            )
        }
    }
}

fn parse<'a>() -> (HashMap<usize, Rule>, Vec<&'a str>) {
    let chunks: Vec<_> = INPUT.split("\n\n").collect();
    let rules: HashMap<usize, Rule> = chunks[0]
        .lines()
        .map(|line| {
            let parts: Vec<_> = line.split(": ").collect();
            (parts[0].parse().unwrap(), Rule::parse(parts[1]))
        })
        .collect();
    (rules, chunks[1].lines().collect())
}

fn matches_rules(rule: usize, rules: &HashMap<usize, Rule>, chars: &[char]) -> Vec<usize> {
    let mut result = vec![];
    match &rules[&rule] {
        Rule::Char(c) => {
            if chars[0] == *c {
                result.push(1);
            }
        }
        Rule::Others(options) => {
            for option in options {
                let mut possibilities: Vec<(&[char], usize)> = vec![(&chars[..], 0)];
                for &option_rule in option {
                    possibilities = possibilities
                        .into_iter()
                        .filter(|(p, _)| !p.is_empty())
                        .flat_map(|(possibility, possibility_len)| {
                            matches_rules(option_rule, rules, possibility)
                                .into_iter()
                                .map(move |len| (&possibility[len..], possibility_len + len))
                        })
                        .collect();
                }
                for (_possibility, possibility_len) in possibilities {
                    result.push(possibility_len);
                }
            }
        }
    }
    result
}

fn solve<'a>(rules: &HashMap<usize, Rule>, lines: &Vec<&'a str>) -> usize {
    lines
        .iter()
        .filter(|&&line| {
            matches_rules(0, &rules, &line.chars().collect::<Vec<_>>()).contains(&line.len())
        })
        .count()
}

fn main() {
    let (mut rules, lines) = parse();
    println!("Part 1: {}", solve(&rules, &lines));

    rules.insert(8, Rule::parse("42 | 42 8"));
    rules.insert(11, Rule::parse("42 31 | 42 11 31"));
    println!("Part 2: {}", solve(&rules, &lines));
}
