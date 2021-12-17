const INPUT: &str = include_str!("../../input/day_17.txt");

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
struct Pos {
    pub x: isize,
    pub y: isize,
}

impl Pos {
    pub fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }
}

fn fly(
    (x_min, x_max): (isize, isize),
    (y_min, y_max): (isize, isize),
    mut velocity: Pos,
) -> Result<isize, isize> {
    let mut pos = Pos::default();
    let mut best_y = 0;
    let mut result = 0;
    while pos.x <= x_max && (pos.y >= y_min || velocity.y >= 0) {
        pos.x += velocity.x;
        pos.y += velocity.y;
        if velocity.x > 0 {
            velocity.x -= 1;
        }
        if velocity.x < 0 {
            velocity.x += 1;
        }
        velocity.y -= 1;

        best_y = best_y.max(pos.y);
        if pos.x >= x_min && pos.x <= x_max {
            if pos.y >= y_min && pos.y <= y_max {
                return Ok(best_y);
            } else if pos.y > y_max {
                result = -1; // Over
            } else if pos.y < y_max {
                result = 1; // Under
            }
        }
    }

    Err(result)
}

fn aim() -> Vec<isize> {
    let input = INPUT.trim().split(": ").nth(1).unwrap();
    let parts: Vec<Vec<isize>> = input
        .split(", ")
        .map(|s| {
            s[2..]
                .split("..")
                .map(|s| s.parse::<isize>().unwrap())
                .collect()
        })
        .collect();
    let x_bounds = (parts[0][0], parts[0][1]);
    let y_bounds = (parts[1][0], parts[1][1]);
    let mut flights = vec![];
    for dx in -200..=200 {
        for dy in -200..=200 {
            let velocity = Pos::new(dx, dy);
            let result = fly(x_bounds, y_bounds, velocity);
            if let Ok(result) = result {
                flights.push(result);
            }
        }
    }
    flights
}

fn main() {
    println!("Part 1: {}", *aim().iter().max().unwrap());
    println!("Part 2: {}", aim().len());
}
