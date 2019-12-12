fn main() {
    let mut velocities = vec![vec![0; 3]; 4];
    let mut moons = vec![
        vec![-16, -1, -12],
        vec![0, -4, -17],
        vec![-11, 11, 0],
        vec![2, 2, -6],
    ];

    let mut first_repeats = vec![0; 3];
    for step in 0_u64.. {
        for m1 in 0..4 {
            for m2 in m1 + 1..4 {
                for i in 0..3 {
                    if moons[m1][i] < moons[m2][i] {
                        velocities[m1][i] += 1;
                        velocities[m2][i] -= 1;
                    } else if moons[m1][i] > moons[m2][i] {
                        velocities[m1][i] -= 1;
                        velocities[m2][i] += 1;
                    }
                }
            }
        }

        for i in 0..3 {
            let mut all_zero = true;
            for m in 0..4 {
                let v = velocities[m][i];
                moons[m][i] += v;
                all_zero = all_zero && v == 0;
            }
            if all_zero && first_repeats[i] == 0 {
                first_repeats[i] = step;
                if first_repeats.iter().all(|&n| n > 0) {
                    let result = calculate_part2(&first_repeats);
                    println!("Part 2 = {}", result);
                    return;
                }
            }
        }

        if step == 999 {
            let mut energy = 0;
            for m in 0..moons.len() {
                energy += moons[m].iter().map(|&n| (n as i32).abs()).sum::<i32>()
                    * velocities[m].iter().map(|&n| (n as i32).abs()).sum::<i32>();
            }
            println!("Part 1 = {}", energy);
        }
    }
}

fn calculate_part2(first_repeats: &[u64]) -> u64 {
    let mut vals: Vec<u64> = first_repeats.to_vec();
    let steps: Vec<u64> = first_repeats.iter().map(|n| n + 1).collect();
    while vals[0] != vals[1] || vals[1] != vals[2] {
        vals[0] += steps[0];
        while vals[1] < vals[0] {
            vals[1] += steps[1];
        }
        while vals[2] < vals[0] {
            vals[2] += steps[2];
        }
    }
    2 * (vals[0] + 1)
}
