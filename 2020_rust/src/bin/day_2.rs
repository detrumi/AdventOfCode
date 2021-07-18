const INPUT: &str = include_str!("../../input/day_2.txt");

fn parse<'a>() -> impl Iterator<Item = (usize, usize, char, &'a str)> {
    INPUT.lines().map(|line| {
        let words: Vec<_> = line.split_ascii_whitespace().collect();
        let lengths: Vec<_> = words[0]
            .split('-')
            .map(|n| n.parse::<usize>().unwrap())
            .collect();
        let value = words[1].trim_end_matches(':').chars().next().unwrap();
        let pass = words[2];
        (lengths[0], lengths[1], value, pass)
    })
}

fn part1() -> usize {
    parse()
        .filter(|(a, b, value, pass)| (a..=b).contains(&&pass.matches(*value).count()))
        .count()
}

fn part2() -> usize {
    parse()
        .filter(|(a, b, value, pass)| {
            let chars: Vec<_> = pass.chars().collect();
            (chars[a - 1] == *value) != (chars[b - 1] == *value)
        })
        .count()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
