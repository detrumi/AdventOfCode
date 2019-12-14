use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Clone, Hash, Debug)]
struct Part {
    chem: String,
    amount: i64,
}

fn main() {
    let file = File::open("input/day_14.txt").unwrap();
    let inputs: Vec<_> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            l.unwrap()
                .replace(",", "")
                .split(' ')
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        })
        .collect();

    let mut mappings: HashMap<String, (i64, Vec<Part>)> = HashMap::new();
    for input in &inputs {
        let num_inputs = input.iter().position(|i| i == "=>").unwrap() / 2;
        let mut reaction_inputs = vec![];
        for i in 0..num_inputs as usize {
            reaction_inputs.push(Part {
                chem: input[2 * i + 1].clone(),
                amount: input[2 * i].parse::<i64>().unwrap(),
            });
        }
        let reaction_output = Part {
            chem: input[input.len() - 1].clone(),
            amount: input[input.len() - 2].parse::<i64>().unwrap(),
        };
        mappings.insert(
            reaction_output.chem,
            (reaction_output.amount, reaction_inputs),
        );
    }

    let part1 = calculate(
        &mappings,
        Part {
            chem: "FUEL".to_string(),
            amount: 1,
        },
        &mut HashMap::new(),
    );
    eprintln!("Part 1 = {:?}", part1);

    let amount = 1935265;
    let part2 = calculate(
        &mappings,
        Part {
            chem: "FUEL".to_string(),
            amount,
        },
        &mut HashMap::new(),
    );
    eprintln!(
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

    let mut result = 0;
    let times = (needed.amount as f32 / *output_amount as f32).ceil() as i64;

    let left_over = times * output_amount - needed.amount;
    if left_over > 0 {
        *leftovers.entry(needed.chem.clone()).or_default() += left_over;
    }

    for input in inputs {
        if input.chem == "ORE" {
            result += times * input.amount;
        } else {
            let left_over = leftovers.entry(input.chem.clone()).or_default();
            let reduced = std::cmp::min(*left_over, times * input.amount);
            if reduced != 0 {
                *left_over -= reduced;
            }

            result += calculate(
                mappings,
                Part {
                    chem: input.chem.clone(),
                    amount: times * input.amount - reduced,
                },
                leftovers,
            );
        }
    }
    result
}
