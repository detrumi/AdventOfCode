use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_4.txt").unwrap();
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .collect();

    let parts: Vec<_> = lines[0]
        .split("-")
        .map(|i| {
            i.chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect::<Vec<usize>>()
        })
        .collect();

    let mut bounds = lines[0].split("-").map(|i| i.parse::<i32>().unwrap());

    let result = test(
        &parts,
        (bounds.next().unwrap(), bounds.next().unwrap()),
        0,
        parts[0][0],
        false,
        String::new(),
    );
    println!("result = {:?}", result);
}

fn test(
    parts: &Vec<Vec<usize>>,
    bounds: (i32, i32),
    d: usize,
    d_min: usize,
    found_same: bool,
    output: String,
) -> (i32, i32) {
    let mut result = (0, 0);
    for i in d_min..=9 {
        let current_digit = std::char::from_digit(i as u32, 10);
        let last_digit = output.chars().last();

        let mut s = output.clone();
        s.push(current_digit.unwrap());
        let new_found_same = found_same || current_digit == last_digit;

        if d == 5 && new_found_same {
            let num = s.parse::<i32>().unwrap();
            if num >= bounds.0 && num < bounds.1 {
                let chars: Vec<char> = s.chars().collect();
                let mut same = false;
                for j in 0..chars.len() - 1 {
                    if chars[j] == chars[j + 1]
                        && (j == 0 || chars[j - 1] != chars[j])
                        && (j == chars.len() - 2 || chars[j + 2] != chars[j])
                    {
                        same = true;
                    }
                }
                result.0 += 1;
                if same {
                    result.1 += 1;
                }
            }
        }
        if d < 5 {
            let test = test(parts, bounds, d + 1, i, new_found_same, s);
            result.0 += test.0;
            result.1 += test.1;
        }
    }
    result
}
