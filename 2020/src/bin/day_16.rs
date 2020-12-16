const INPUT: &str = include_str!("../../input/day_16.txt");

fn parse() -> (Vec<Vec<Vec<usize>>>, Vec<usize>, Vec<Vec<usize>>) {
    let chunks: Vec<_> = INPUT.split("\n\n").collect();
    let parse_nums = |s: &str| s.split(',').map(|s| s.parse().unwrap()).collect();

    let fields = chunks[0]
        .lines()
        .map(|line| {
            line.split(": ")
                .nth(1)
                .unwrap()
                .split(" or ")
                .map(|r| r.split("-").map(|n| n.parse().unwrap()).collect())
                .collect()
        })
        .collect();
    let my_ticket = parse_nums(chunks[1].lines().nth(1).unwrap());
    let nearby = chunks[2].lines().skip(1).map(parse_nums).collect();

    (fields, my_ticket, nearby)
}

fn part1() -> usize {
    let (fields, _, nearby) = parse();
    nearby
        .iter()
        .flat_map(|ticket| ticket.iter())
        .filter(|&&n| {
            fields
                .iter()
                .all(|f| f.iter().all(|range| n < range[0] || n > range[1]))
        })
        .sum()
}

fn find_order(
    possibilities: &Vec<Vec<usize>>,
    found: &mut Vec<usize>,
    index: usize,
) -> Option<Vec<usize>> {
    for n in &possibilities[index] {
        if !found.contains(n) {
            found.push(*n);
            if found.len() == possibilities.len() {
                return Some(found.clone());
            }
            if let Some(order) = find_order(possibilities, found, index + 1) {
                return Some(order);
            }
            found.pop();
        }
    }
    None
}

fn part2() -> usize {
    let (fields, my_ticket, nearby) = parse();
    let valids: Vec<Vec<Vec<bool>>> = nearby
        .iter()
        .map(|numbers| {
            numbers
                .iter()
                .map(|&n| {
                    fields
                        .iter()
                        .map(|f| f.iter().any(|range| n >= range[0] && n <= range[1]))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .filter(|ticket| ticket.iter().all(|bools| bools.iter().any(|&b| b)))
        .collect();

    let possibilities: Vec<Vec<usize>> = (0..valids[0].len())
        .map(|i| {
            (0..valids[0].len())
                .filter(|&j| valids.iter().all(|ticket| ticket[i][j]))
                .collect()
        })
        .collect();

    let mut found = vec![];
    find_order(&possibilities, &mut found, 0)
        .unwrap()
        .iter()
        .enumerate()
        .filter(|(_, n)| **n < 6)
        .map(|(i, _)| my_ticket[i])
        .product()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
