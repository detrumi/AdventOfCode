const INPUT: &str = include_str!("../../input/day_4.txt");

type Card = Vec<Vec<Option<usize>>>;

fn has_won(card: &Card) -> bool {
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

fn wins(draws: Vec<usize>, mut cards: Vec<Card>) -> Vec<usize> {
    let mut result = vec![];
    let mut won_cards = vec![false; cards.len()];
    for draw in draws {
        for (i, card) in &mut cards.iter_mut().enumerate() {
            if won_cards[i] {
                continue;
            }
            for y in 0..5 {
                for x in 0..5 {
                    if card[y][x] == Some(draw) {
                        card[y][x] = None;
                        if has_won(&card) {
                            let points: usize = card
                                .iter()
                                .map(|row| row.iter().filter_map(|o| *o).sum::<usize>())
                                .sum();
                            result.push(points * draw);
                            won_cards[i] = true;
                        }
                    }
                }
            }
        }
    }
    result
}

fn main() {
    let parts: Vec<_> = INPUT.trim().split("\n\n").collect();
    let draws: Vec<usize> = parts[0].split(',').map(|s| s.parse().unwrap()).collect();
    let cards: Vec<Card> = parts[1..]
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

    let wins = wins(draws, cards);
    println!("Part 1: {}", wins[0]);
    println!("Part 2: {}", wins.last().unwrap());
}
