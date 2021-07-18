#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Default)]
struct Pos {
    x: i32,
    y: i32,
}

impl Pos {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

const INPUT: i32 = 9221;

fn main() {
    let mut cells = vec![vec![0; 300]; 300];
    for y in 0..300 {
        for x in 0..300 {
            let rack_id = x + 11;
            let mut power = rack_id * (y + 1);
            power += INPUT;
            power *= rack_id;
            power = (power / 100) % 10;
            power -= 5;
            cells[y as usize][x as usize] = power;
        }
    }

    part_1(&cells);
    part_2(&cells);
}

fn part_1(cells: &Vec<Vec<i32>>) {
    let mut largest = (Pos::new(0, 0), std::i32::MIN);
    for y in 0..298 {
        for x in 0..298 {
            let mut sum = 0;
            for dy in 0..3 {
                for dx in 0..3 {
                    sum += cells[y + dy][x + dx];
                }
            }
            if sum > largest.1 {
                largest = (Pos::new(x as i32, y as i32), sum);
            }
        }
    }
    println!("Part 1: {},{}", largest.0.x + 1, largest.0.y + 1,);
}

fn part_2(cells: &Vec<Vec<i32>>) {
    let mut largest = (Pos::new(0, 0), 0, std::i32::MIN);
    for y in 0..300 {
        println!("{}", y);
        for x in 0..300 {
            for size in 1..(300 - x).min(300 - y) {
                let mut sum = 0;
                for dy in 0..size {
                    for dx in 0..size {
                        sum += cells[y + dy][x + dx];
                    }
                }
                if sum > largest.2 {
                    largest = (Pos::new(x as i32, y as i32), size, sum);
                }
            }
        }
    }
    println!(
        "Part 2: {},{},{}",
        largest.0.x + 1,
        largest.0.y + 1,
        largest.1
    );
}
