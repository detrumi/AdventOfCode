use serde_json::Value;

const INPUT: &str = include_str!("../../input/day_12.txt");

fn sum_numbers(value: &Value) -> i64 {
    match value {
        Value::Null | Value::Bool(_) | Value::String(_) => 0,
        Value::Number(n) => n.as_i64().unwrap(),
        Value::Array(a) => a.iter().map(sum_numbers).sum(),
        Value::Object(o) => o.values().map(sum_numbers).sum(),
    }
}

fn sum_non_red_numbers(value: &Value) -> i64 {
    match value {
        Value::Null | Value::Bool(_) | Value::String(_) => 0,
        Value::Number(n) => n.as_i64().unwrap(),
        Value::Array(a) => a.iter().map(sum_non_red_numbers).sum(),
        Value::Object(o) => {
            if o.values().any(|v| *v == Value::String("red".to_string())) {
                0
            } else {
                o.values().map(sum_non_red_numbers).sum()
            }
        }
    }
}

fn part1() -> i64 {
    sum_numbers(&serde_json::from_str(INPUT).unwrap())
}

fn part2() -> i64 {
    sum_non_red_numbers(&serde_json::from_str(INPUT).unwrap())
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
