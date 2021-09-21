use bitvec::bitvec;

fn main() {
    let mut state = 0;
    let mut tape = bitvec![0; 10_000];
    let mut i: i32 = tape.len() as i32 / 2;
    for _ in 0..12656374 {
        let (value, offset, new_state) = match (state, tape[i as usize] as u32) {
            (0, 0) => (1, 1, 1),
            (0, 1) => (0, -1, 2),
            (1, 0) => (1, -1, 0),
            (1, 1) => (1, -1, 3),
            (2, 0) => (1, 1, 3),
            (2, 1) => (0, 1, 2),
            (3, 0) => (0, -1, 1),
            (3, 1) => (0, 1, 4),
            (4, 0) => (1, 1, 2),
            (4, 1) => (1, -1, 5),
            (5, 0) => (1, -1, 4),
            (5, 1) => (1, 1, 0),
            _ => unreachable!(),
        };
        tape.set(i as usize, value != 0);
        i += offset;
        state = new_state;
    }
    println!("Part 1: {}", tape.count_ones());
}
