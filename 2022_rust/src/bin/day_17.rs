use std::collections::VecDeque;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_17.txt");

const WIDTH: usize = 7;

fn solve(num_rocks: usize) -> usize {
    let pieces: [Vec<(usize, usize)>; 5] = [
        vec![(0, 0), (1, 0), (2, 0), (3, 0)],
        vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
        vec![(2, 0), (2, 1), (0, 2), (1, 2), (2, 2)],
        vec![(0, 0), (0, 1), (0, 2), (0, 3)],
        vec![(0, 0), (1, 0), (0, 1), (1, 1)],
    ];
    let piece_sizes = pieces
        .iter()
        .map(|piece| {
            (
                piece.iter().map(|pos| pos.0).max().unwrap() + 1,
                piece.iter().map(|pos| pos.1).max().unwrap() + 1,
            )
        })
        .collect_vec();

    let mut rocks: VecDeque<[bool; WIDTH]> = VecDeque::new();

    let mut piece_index = pieces.len() - 1;
    let mut pattern = INPUT.trim().chars().cycle();
    let mut height = 0;

    let mut height_from_cycles = 0;
    let mut placements = vec![];
    let mut heights = vec![];
    let mut i = 0;
    while i < num_rocks {
        i += 1;
        piece_index = (piece_index + 1) % pieces.len();
        let piece = &pieces[piece_index];
        let piece_size = &piece_sizes[piece_index];

        let highest = height
            - rocks
                .iter()
                .position(|row| row.iter().any(|b| *b))
                .unwrap_or_default();
        while height > piece_size.1 + highest + 3 {
            rocks.pop_front();
            height -= 1;
        }
        while height < piece_size.1 + highest + 3 {
            rocks.push_front([false; WIDTH]);
            height += 1;
        }

        let mut pos = (2_usize, 0_usize);
        let mut first = true;
        loop {
            if first {
                first = false;
            } else if pos.1 + piece_size.1 < rocks.len()
                && piece
                    .iter()
                    .all(|p| !rocks[pos.1 + p.1 + 1][pos.0 + p.0 as usize])
            {
                pos.1 += 1;
            } else {
                placements.push(pos.0);
                for p in piece {
                    rocks[pos.1 + p.1][pos.0 + p.0] = true;
                }
                break;
            }

            let dx = match pattern.next().unwrap() {
                '<' => -1,
                '>' => 1,
                _ => panic!(),
            };
            if pos.0 as isize + dx >= 0
                && pos.0 + ((piece_size.0 as isize + dx) as usize) <= WIDTH
                && piece
                    .iter()
                    .all(|p| !rocks[pos.1 + p.1][((pos.0 + p.0) as isize + dx) as usize])
            {
                pos.0 = (pos.0 as isize + dx) as usize;
            }
        }

        const MIN_CYCLE_LEN: usize = 10;
        if height_from_cycles == 0 {
            let len = placements.len();
            for cycle_len in MIN_CYCLE_LEN..len / 2 {
                if placements[len - cycle_len..] == placements[len - 2 * cycle_len..len - cycle_len]
                {
                    let increase = height - heights[heights.len() - cycle_len];
                    let cycles_left = (num_rocks - i) / cycle_len;
                    height_from_cycles = increase * cycles_left;
                    i += cycles_left * cycle_len;
                    break;
                }
            }
            heights.push(height);
        }
    }
    height - rocks.iter().position(|row| row.iter().any(|b| *b)).unwrap() + height_from_cycles
}

fn main() {
    println!("Part 1: {}", solve(2022));
    println!("Part 2: {}", solve(1_000_000_000_000));
}
