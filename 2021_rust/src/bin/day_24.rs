const V1: [i64; 14] = [12, 11, 12, -3, 10, -9, 10, -7, -11, -4, 14, 11, -8, -10];
const V2: [i64; 14] = [7, 15, 2, 15, 14, 2, 15, 1, 15, 15, 12, 2, 13, 13];

fn solve_round(input: i64, mut z: i64, i: usize) -> i64 {
    let x = z % 26;
    if [3, 5, 7, 8, 9, 12, 13].contains(&i) {
        z /= 26;
    }

    if x + V1[i] != input {
        z *= 26;
        z += input + V2[i];
    }
    z
}

fn solve1(z: i64, i: usize, order: &Vec<i64>) -> Option<String> {
    for n in order {
        let z_new = solve_round(*n, z, i);
        if i == 13 {
            if z_new == 0 {
                return Some(n.to_string());
            }
        } else if i != 9 || z_new == 0 {
            if let Some(s) = solve1(z_new, i + 1, order) {
                return Some(format!("{}{}", n, s));
            }
        }
    }
    None
}

fn solve(order: impl Iterator<Item = i64>) -> String {
    solve1(0, 0, &order.collect()).unwrap()
}

fn main() {
    println!("Part 1: {}", solve((1..=9).rev()));
    println!("Part 2: {}", solve(1..=9));
}
