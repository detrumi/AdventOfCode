use std::collections::HashSet;

const INPUT: &str = include_str!("../../input/day_8.txt");

fn part1() -> i32 {
    let mut input = vec![];
    for line in INPUT.lines() {
        let parts: Vec<_> = line.split_ascii_whitespace().collect();
        let instr = parts[0];
        let num = parts[1].parse::<i32>().unwrap();
        input.push((instr, num));
    }

    let mut i = 0_i32;
    let mut acc = 0;
    let mut executed = HashSet::new();
    loop {
        let (instr, num) = input[i as usize];
        if executed.contains(&i) {
            break;
        }
        executed.insert(i);
        match instr {
            "acc" => acc += num,
            "jmp" => i += num - 1,
            "nop" => (),
            _ => panic!(),
        }
        i += 1;
    }

    acc
}

fn part2() -> i32 {
    let mut initial_input = vec![];
    for line in INPUT.lines() {
        let parts: Vec<_> = line.split_ascii_whitespace().collect();
        let instr = parts[0];
        let num = parts[1].parse::<i32>().unwrap();
        initial_input.push((instr, num));
    }

    'outer: for (corrupt_index, (instr, _num)) in initial_input.iter().enumerate() {
        let mut input = initial_input.clone();
        match *instr {
            "acc" => continue,
            "jmp" => input[corrupt_index].0 = "nop",
            "nop" => input[corrupt_index].0 = "jmp",
            _ => panic!(),
        }

        let mut i = 0_i32;
        let mut acc = 0;
        let mut executed = HashSet::new();
        loop {
            if i as usize >= input.len() {
                return acc;
            }
            let (instr, num) = input[i as usize];
            if executed.contains(&i) {
                continue 'outer;
            }
            executed.insert(i);
            match instr {
                "acc" => acc += num,
                "jmp" => i += num - 1,
                "nop" => (),
                _ => panic!(),
            }
            i += 1;
        }
    }
    unreachable!()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
