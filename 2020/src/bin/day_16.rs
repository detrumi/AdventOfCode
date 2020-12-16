const INPUT: &str = include_str!("../../input/day_16.txt");

fn part1() -> usize {
    let chunks: Vec<_> = INPUT.split("\n\n").collect();
    let mut fields: Vec<Vec<Vec<usize>>> = vec![];
    for line in chunks[0].lines() {
        let parts: Vec<_> = line.split(": ").collect();
        fields.push(
            parts[1]
                .split(" or ")
                .map(|r| r.split("-").map(|n| n.parse::<usize>().unwrap()).collect())
                .collect(),
        );
    }

    let mut result = 0;
    for line in chunks[2].lines().skip(1) {
        'outer: for number in line.split(',').map(|s| s.parse::<usize>().unwrap()) {
            for field in &fields {
                if field
                    .iter()
                    .any(|range| number >= range[0] && number <= range[1])
                {
                    continue 'outer;
                }
            }
            result += number;
        }
    }
    result
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
    let chunks: Vec<_> = INPUT.split("\n\n").collect();
    let mut fields: Vec<Vec<Vec<usize>>> = vec![];
    for line in chunks[0].lines() {
        let parts: Vec<_> = line.split(": ").collect();
        fields.push(
            parts[1]
                .split(" or ")
                .map(|r| r.split("-").map(|n| n.parse::<usize>().unwrap()).collect())
                .collect(),
        );
    }

    let valids: Vec<Vec<Vec<bool>>> = chunks[2]
        .lines()
        .skip(1)
        .map(|line| {
            line.split(',')
                .map(|s| s.parse::<usize>().unwrap())
                .map(|n| {
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
    let order = find_order(&possibilities, &mut found, 0);

    let my_ticket: Vec<usize> = chunks[1]
        .lines()
        .nth(1)
        .unwrap()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();

    order
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
