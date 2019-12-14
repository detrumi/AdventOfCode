use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Clone, Hash, Debug)]
struct Part {
    chem: String,
    amount: i64,
}

impl Part {
    pub fn new(s: &str, amount: i64) -> Self {
        Part {
            chem: String::from(s),
            amount,
        }
    }
}

fn main() {
    let file = File::open("input/day_14.txt").unwrap();
    let mappings: HashMap<String, (i64, Vec<Part>)> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            let parts = l
                .unwrap()
                .replace(",", "")
                .split(' ')
                .map(|s| s.to_string())
                .collect::<Vec<_>>();

            let num_inputs = parts.iter().position(|i| i == "=>").unwrap() / 2_usize;
            let inputs: Vec<Part> = (0..num_inputs)
                .map(|i| Part::new(&parts[2 * i + 1], parts[2 * i].parse::<i64>().unwrap()))
                .collect();

            let output_chem = parts[parts.len() - 1].clone();
            let output_amount = parts[parts.len() - 2].parse::<i64>().unwrap();

            (output_chem, (output_amount, inputs))
        })
        .collect();

    let part1 = calculate(&mappings, Part::new("FUEL", 1), &mut HashMap::new());
    println!("Part 1 = {:?}", part1);

    let amount = 1935265;
    let part2 = calculate(&mappings, Part::new("FUEL", amount), &mut HashMap::new());
    println!(
        "Part 2 = {}, with {} left over",
        amount,
        1_000_000_000_000 - part2
    );
}

fn calculate(
    mappings: &HashMap<String, (i64, Vec<Part>)>,
    needed: Part,
    leftovers: &mut HashMap<String, i64>,
) -> i64 {
    let (output_amount, inputs) = &mappings[&needed.chem];
    let times = (needed.amount as f32 / *output_amount as f32).ceil() as i64;
    *leftovers.entry(needed.chem.clone()).or_default() += times * output_amount - needed.amount;
    inputs
        .iter()
        .map(|input| {
            if input.chem == "ORE" {
                times * input.amount
            } else {
                let left_over = leftovers.entry(input.chem.clone()).or_default();
                let reduced = std::cmp::min(*left_over, times * input.amount);
                *left_over -= reduced;

                calculate(
                    mappings,
                    Part::new(&input.chem, times * input.amount - reduced),
                    leftovers,
                )
            }
        })
        .sum()
}
