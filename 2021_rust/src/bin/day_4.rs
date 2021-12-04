const INPUT: &str = include_str!("../../input/day_4.txt");

fn has_won(card: &Vec<Vec<Option<usize>>>) -> bool {
    for y in 0..5 {
        if (0..5).all(|x| card[y][x].is_none()) {
            return true;
        }
    }
    for x in 0..5 {
        if (0..5).all(|y| card[y][x].is_none()) {
            return true;
        }
    }

    false
}

fn part1() -> usize {
    let parts: Vec<_> = INPUT.trim().split("\n\n").collect();
    let draws: Vec<usize> = parts[0].split(',').map(|s| s.parse().unwrap()).collect();
    let mut cards: Vec<Vec<Vec<Option<usize>>>> = parts[1..]
        .iter()
        .map(|card| {
            card.lines()
                .map(|line| {
                    line.split_ascii_whitespace()
                        .map(|s| s.parse().ok())
                        .collect()
                })
                .collect()
        })
        .collect();

    let mut won_cards = vec![false; cards.len()];
    for n in draws {
        for (i, card) in &mut cards.iter_mut().enumerate() {
            if won_cards[i] {
                continue;
            }
            for y in 0..5 {
                for x in 0..5 {
                    if card[y][x] == Some(n) {
                        card[y][x] = None;
                        if has_won(&card) {
                            let points: usize = card
                                .iter()
                                .map(|row| row.iter().filter_map(|o| *o).sum::<usize>())
                                .sum();
                            println!("Win: {}", points * n);
                            won_cards[i] = true;
                        }
                    }
                }
            }
        }
    }
    0
}

fn part2() -> usize {
    0
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
