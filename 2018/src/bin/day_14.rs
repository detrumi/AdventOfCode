fn main() {
    calculate(236_021);
}

fn calculate(input: usize) {
    let (mut index1, mut index2) = (0, 1);
    let (mut current1, mut current2) = (3, 7);
    let mut score_board: Vec<usize> = vec![3, 7];
    let mut found_part_1 = false;

    let search_for: Vec<usize> = input
        .to_string()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as usize)
        .collect();
    let mut search_index = 0;
    let mut check_part_2 = |n: usize, len: usize| {
        if n == search_for[search_index] {
            search_index += 1;
            if search_index == search_for.len() {
                println!("Part 2: {}", len - search_index + 1);
                return true;
            }
        } else {
            search_index = 0;
        }
        false
    };

    loop {
        let new_digits = (current1 + current2)
            .to_string()
            .chars()
            .map(|c| c.to_digit(10).unwrap() as usize)
            .collect::<Vec<_>>();

        let len = score_board.len();
        if check_part_2(new_digits[0], len) { return; }
        score_board.push(new_digits[0]);
        if new_digits.len() > 1 {
            if check_part_2(new_digits[1], len) { return; }
            score_board.push(new_digits[1]);
        }

        index1 = (index1 + score_board[index1] + 1) % score_board.len();
        current1 = score_board[index1];

        index2 = (index2 + score_board[index2] + 1) % score_board.len();
        current2 = score_board[index2];

        if !found_part_1 && score_board.len() >= input + 10 {
            let mut last = score_board.len();
            last -= if score_board.len() > input + 10 { 1 } else { 0 };
            let result = score_board[score_board.len() - 10..last]
                .iter()
                .map(|n| n.to_string())
                .collect::<String>();
            println!("Part 1: {}", result);
            found_part_1 = true;
        }
    }
}
