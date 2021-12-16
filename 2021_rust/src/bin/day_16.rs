const INPUT: &str = include_str!("../../input/day_16.txt");

fn read_bits(bits: &Vec<char>, ptr: &mut usize, len: usize) -> usize {
    let result =
        usize::from_str_radix(&bits[*ptr..*ptr + len].iter().collect::<String>(), 2).unwrap();
    *ptr += len;
    result
}

fn read_bits_raw(bits: &Vec<char>, ptr: &mut usize, len: usize) -> (String, bool) {
    let is_last = bits[*ptr] == '0';
    let result = bits[*ptr + 1..*ptr + len].iter().collect::<String>();
    *ptr += len;
    (result, is_last)
}

fn read_packet(bits: &Vec<char>, ptr: &mut usize) -> usize {
    let mut result = read_bits(&bits, ptr, 3);
    let type_id = read_bits(&bits, ptr, 3);
    if type_id == 4 {
        loop {
            let (_group, is_last) = read_bits_raw(&bits, ptr, 5);
            if is_last {
                break;
            }
        }
    } else {
        if read_bits(&bits, ptr, 1) == 0 {
            let len = read_bits(&bits, ptr, 15);
            let start = *ptr;
            while *ptr - start < len {
                result += read_packet(&bits, ptr);
            }
        } else {
            let num_subpackets = read_bits(&bits, ptr, 11);
            for _ in 0..num_subpackets {
                result += read_packet(bits, ptr);
            }
        }
    }
    result
}

fn part1() -> usize {
    let hex = hex::decode(INPUT.trim()).unwrap();
    let bits: Vec<char> = hex
        .iter()
        .flat_map(|byte| {
            format!("{:08b}", byte)
                .chars()
                .collect::<Vec<_>>()
                .into_iter()
        })
        .collect();
    read_packet(&bits, &mut 0)
}

fn read_packet2(bits: &Vec<char>, ptr: &mut usize) -> usize {
    let _version = read_bits(&bits, ptr, 3);
    let type_id = read_bits(&bits, ptr, 3);
    if type_id == 4 {
        let mut result = String::new();
        loop {
            let (group, is_last) = read_bits_raw(&bits, ptr, 5);
            result += &group;
            if is_last {
                break;
            }
        }
        usize::from_str_radix(&result, 2).unwrap()
    } else {
        let mut packets = vec![];
        if read_bits(&bits, ptr, 1) == 0 {
            let len = read_bits(&bits, ptr, 15);
            let start = *ptr;
            while *ptr - start < len {
                packets.push(read_packet2(&bits, ptr));
            }
        } else {
            let num_subpackets = read_bits(&bits, ptr, 11);
            for _ in 0..num_subpackets {
                packets.push(read_packet2(bits, ptr));
            }
        }
        match type_id {
            0 => packets.iter().sum(),
            1 => packets.iter().product(),
            2 => *packets.iter().min().unwrap(),
            3 => *packets.iter().max().unwrap(),
            5 => (packets[0] > packets[1]) as usize,
            6 => (packets[0] < packets[1]) as usize,
            7 => (packets[0] == packets[1]) as usize,
            _ => panic!(),
        }
    }
}

fn part2() -> usize {
    let hex = hex::decode(INPUT.trim()).unwrap();
    let bits: Vec<char> = hex
        .iter()
        .flat_map(|byte| {
            format!("{:08b}", byte)
                .chars()
                .collect::<Vec<_>>()
                .into_iter()
        })
        .collect();
    read_packet2(&bits, &mut 0)
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
