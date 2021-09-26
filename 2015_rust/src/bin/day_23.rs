const INPUT: &str = include_str!("../../input/day_23.txt");

type Register = usize;

fn parse_register(s: &str) -> Register {
    s.chars().next().unwrap() as usize - 'a' as usize
}

fn parse() -> Vec<Instruction> {
    INPUT.trim().lines().map(Instruction::from).collect()
}

enum Instruction {
    Half(Register),
    Triple(Register),
    Increment(Register),
    Jump(i32),
    JumpIfEven(Register, i32),
    JumpIfOne(Register, i32),
}

impl From<&str> for Instruction {
    fn from(s: &str) -> Self {
        let s = s.replace(",", "");
        let parts: Vec<_> = s.split(" ").into_iter().collect();
        match parts[0] {
            "hlf" => Instruction::Half(parse_register(parts[1])),
            "tpl" => Instruction::Triple(parse_register(parts[1])),
            "inc" => Instruction::Increment(parse_register(parts[1])),
            "jmp" => Instruction::Jump(parts[1].parse::<i32>().unwrap()),
            "jie" => {
                Instruction::JumpIfEven(parse_register(parts[1]), parts[2].parse::<i32>().unwrap())
            }
            "jio" => {
                Instruction::JumpIfOne(parse_register(parts[1]), parts[2].parse::<i32>().unwrap())
            }
            _ => panic!(),
        }
    }
}

fn run(mut registers: [usize; 2]) -> usize {
    let instructions = parse();
    let mut i = 0_i32;
    while i >= 0 && i < instructions.len() as i32 {
        match instructions[i as usize] {
            Instruction::Half(r) => registers[r] /= 2,
            Instruction::Triple(r) => registers[r] *= 3,
            Instruction::Increment(r) => registers[r] += 1,
            Instruction::Jump(n) => i += n - 1,
            Instruction::JumpIfEven(r, n) if registers[r] % 2 == 0 => i += n - 1,
            Instruction::JumpIfOne(r, n) if registers[r] == 1 => i += n - 1,
            _ => (),
        }
        i += 1
    }
    registers[1]
}

fn main() {
    println!("Part 1: {}", run([0, 0]));
    println!("Part 2: {}", run([1, 0]));
}
