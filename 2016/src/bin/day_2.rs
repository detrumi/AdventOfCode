use std::fs::File;
use std::io::*;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
struct Pos {
    x: i32,
    y: i32,
}

fn key(p: Pos) -> String {
    match p.y {
        -2 => "1".to_string(),
        -1 => format!("{}", p.x + 3),
        0 => format!("{}", p.x + 7),
        1 => match p.x {
            -1 => "A".to_string(),
            0 => "B".to_string(),
            1 => "C".to_string(),
            _ => "?".to_string(),
        }
        2 => "D".to_string(),
        _ => "?".to_string(),
    }
}

fn main() {
    let mut file = File::open("input/day_2.txt").expect("Unable to open file");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("Unable to read file");

    find_code(Pos { x: 1, y: 1 }, &input, |c, pos| {
        match c {
            'U' => if pos.y > 0 { pos.y -= 1 },
            'R' => if pos.x < 2 { pos.x += 1 },
            'D' => if pos.y < 2 { pos.y += 1 },
            'L' => if pos.x > 0 { pos.x -= 1 },
            _ => (),
        }
    }, |pos| { format!("{}", 3 * pos.y + pos.x + 1) });

    find_code(Pos { x: -2, y: 0 }, &input, |c, pos| {
        match c {
            'U' => if pos.y > pos.x.abs() - 2 { pos.y -= 1 },
            'R' => if pos.x < 2 - pos.y.abs() { pos.x += 1 },
            'D' => if pos.y < 2 - pos.x.abs() { pos.y += 1 },
            'L' => if pos.x > pos.y.abs() - 2 { pos.x -= 1 },
            _ => (),
        }
    }, key);
}

fn find_code<F, G>(mut pos: Pos, input: &str, mover: F, key_fn: G)
    where F: Fn(char, &mut Pos),
          G: Fn(Pos) -> String {
    let mut code = String::new();
    for line in input.lines() {
        for c in line.chars() {
            mover(c, &mut pos);
        }
        code.push_str(&key_fn(pos));
    }
    println!("Code: {}", code);

}
