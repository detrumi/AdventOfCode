const INPUT: &str = include_str!("../../input/day_10.txt");

fn main() {
    let mut error_score = 0;
    let mut scores = vec![];
    'outer: for line in INPUT.trim().lines() {
        let mut opened = vec![];
        for c in line.chars() {
            match c {
                '(' | '[' | '{' | '<' => opened.push(c),
                _ => {
                    let (matching, score) = match c {
                        ')' => ('(', 3),
                        ']' => ('[', 57),
                        '}' => ('{', 1197),
                        '>' => ('<', 25137),
                        _ => panic!(),
                    };
                    if opened.pop() != Some(matching) {
                        error_score += score;
                        continue 'outer;
                    }
                }
            }
        }
        let mut score: usize = 0;
        while let Some(c) = opened.pop() {
            score *= 5;
            score += match c {
                '(' => 1,
                '[' => 2,
                '{' => 3,
                '<' => 4,
                _ => panic!(),
            }
        }
        scores.push(score);
    }

    scores.sort();
    println!("Part 1: {}", error_score);
    println!("Part 2: {}", scores[scores.len() / 2]);
}
