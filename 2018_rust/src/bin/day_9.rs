use std::collections::VecDeque;

fn main() {
    // calculate(9, 25);
    calculate(416, 71975);
    calculate(416, 7197500);
}

fn calculate(players: i32, last_marble_points: i32) -> Option<()> {
    let mut marbles = VecDeque::new();
    marbles.push_back(0);
    let mut scores = vec![0; players as usize];
    for i in 1..=last_marble_points {
        let front = marbles.pop_front()?;
        marbles.push_back(front);

        if i % 23 == 0 {
            for _ in 0..=7 {
                let elem = marbles.pop_back()?;
                marbles.push_front(elem);
            }
            let removed = marbles.pop_back()?;
            scores[(i % players) as usize] += i as usize + removed;

            let first = marbles.pop_front()?;
            marbles.push_back(first);
        } else {
            marbles.push_back(i as usize);
        }
    }

    println!("Answer: {}", scores.iter().max()?);
    Some(())
}
