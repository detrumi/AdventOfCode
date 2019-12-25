use std::fs::File;
use std::io::{self, BufRead};
use std::sync::mpsc;
use std::thread::{self, JoinHandle};
use std::time;

fn main() {
    let file = File::open("input/day_23.txt").unwrap();
    let mem: Vec<i64> = io::BufReader::new(file)
        .lines()
        .map(|l| {
            l.unwrap()
                .split(',')
                .map(|s| s.parse::<i64>().unwrap())
                .collect()
        })
        .next()
        .unwrap();

    let (nat_tx, nat_rx) = mpsc::channel();
    let mut receivers = vec![];
    let mut tx_0 = None;
    let programs: Vec<Program> = (0..50)
        .map(|n| {
            let (tx, rx) = mpsc::channel();
            if n == 0 {
                tx_0 = Some(tx.clone());
            }
            tx.send((n, n)).unwrap();
            receivers.push(tx);
            Program::new(mem.clone(), rx, nat_tx.clone())
        })
        .collect();

    let _handlers: Vec<JoinHandle<_>> = programs
        .into_iter()
        .map(move |mut program| {
            let recv = receivers.clone();
            thread::spawn(move || {
                program.output = recv;
                program.calculate();
            })
        })
        .collect();

    let mut idle_programs = vec![false; 50];
    let mut last_value = None;
    let mut previous_y = 0;
    let tx_0 = tx_0.unwrap();
    loop {
        while let Ok((addr, val)) = nat_rx.try_recv() {
            if let Some(addr) = addr {
                idle_programs[addr as usize] = val == (0, 0);
            } else {
                eprintln!("NAT last value = {:?}", val);
                last_value = Some(val);
                idle_programs = vec![false; 50];
            }
        }

        if let Some(last) = last_value {
            if idle_programs.iter().all(|idle| *idle) {
                eprintln!("Sending to 0: {:?}", last_value.unwrap());
                if last.1 == previous_y {
                    eprintln!("Repeated: {}", last.1);
                }
                previous_y = last.1;
                tx_0.send(last).unwrap();
                idle_programs = vec![false; 50];
            }
        }
    }
}

struct Program {
    i: usize,
    instruction: i64,
    relative_base: i64,
    mem: Vec<i64>,
    input: mpsc::Receiver<(i64, i64)>,
    pub output: Vec<mpsc::Sender<(i64, i64)>>,
    nat_rx: mpsc::Sender<(Option<i64>, (i64, i64))>,
}

impl Program {
    fn new(
        mem: Vec<i64>,
        input: mpsc::Receiver<(i64, i64)>,
        nat_rx: mpsc::Sender<(Option<i64>, (i64, i64))>,
    ) -> Self {
        Self {
            i: 0,
            instruction: 0,
            relative_base: 0,
            mem,
            input,
            output: vec![],
            nat_rx,
        }
    }

    fn calculate(&mut self) {
        let mut own_address: Option<i64> = None;
        let mut destination_address: Option<i64> = None;
        let mut x_write_value = None;
        let mut y_read_value = None;
        let mut time_idle = 0;
        while let Some(instruction) = self.mem.get(self.i) {
            self.instruction = *instruction;
            thread::sleep(time::Duration::from_millis(5));

            match self.instruction % 100 {
                1 => {
                    let value = self.param(1) + self.param(2);
                    self.write(3, value);
                    self.i += 4;
                }
                2 => {
                    let value = self.param(1) * self.param(2);
                    self.write(3, value);
                    self.i += 4;
                }
                3 => {
                    if own_address.is_none() {
                        let values = self.input.recv().unwrap();
                        own_address = Some(values.0);
                        self.write(1, values.0);
                    } else if let Some(y) = y_read_value {
                        self.write(1, y);
                        y_read_value = None;
                    } else {
                        let values = if let Ok(val) = self.input.try_recv() {
                            if time_idle > 0 {
                                time_idle = 0;
                                self.nat_rx
                                    .send((own_address, (time_idle as i64, time_idle as i64)))
                                    .unwrap();
                            }
                            val
                        } else {
                            time_idle += 1;
                            self.nat_rx.send((own_address, (0, 0))).unwrap();
                            (-1, -1)
                        };

                        self.write(1, values.0);
                        y_read_value = Some(values.1);
                    }
                    self.i += 2;
                }
                4 => {
                    if time_idle > 0 {
                        time_idle = 0;
                        self.nat_rx
                            .send((own_address, (time_idle as i64, time_idle as i64)))
                            .unwrap();
                    }
                    let value = self.param(1);
                    if let Some(address) = destination_address {
                        if let Some(x) = x_write_value {
                            let values = (x, value);
                            if address == 255 {
                                self.nat_rx.send((None, values)).unwrap();
                            } else {
                                self.output[address as usize].send(values).unwrap();
                            }
                            destination_address = None;
                            x_write_value = None;
                        } else {
                            x_write_value = Some(value);
                        }
                    } else {
                        destination_address = Some(value);
                    }
                    self.i += 2;
                }
                5 => {
                    self.i = if self.param(1) != 0 {
                        self.param(2) as usize
                    } else {
                        self.i + 3
                    }
                }
                6 => {
                    self.i = if self.param(1) == 0 {
                        self.param(2) as usize
                    } else {
                        self.i + 3
                    };
                }
                7 => {
                    let value = (self.param(1) < self.param(2)) as i64;
                    self.write(3, value);
                    self.i += 4;
                }
                8 => {
                    let value = (self.param(1) == self.param(2)) as i64;
                    self.write(3, value);
                    self.i += 4;
                }
                9 => {
                    self.relative_base += self.param(1);
                    self.i += 2;
                }
                99 => return,
                n => panic!(format!("{}", n)),
            }
        }
    }

    fn target(&self, index: usize) -> usize {
        match self.mode(index) {
            0 => self.mem[self.i + index] as usize,
            1 => self.i + index,
            2 => (self.relative_base + self.mem[(self.i + index) as usize]) as usize,
            _ => panic!(),
        }
    }

    fn mode(&self, index: usize) -> i64 {
        match index {
            1 => (self.instruction / 100) % 10,
            2 => (self.instruction / 1_000) % 10,
            3 => (self.instruction / 10_000) % 10,
            _ => panic!(),
        }
    }

    fn param(&mut self, index: usize) -> i64 {
        let target = self.target(index);
        if target >= self.mem.len() {
            self.mem.resize(target + 1, 0);
        }
        self.mem[target]
    }

    fn write(&mut self, index: usize, value: i64) {
        let target = self.target(index);
        if target >= self.mem.len() {
            self.mem.resize(target + 1, 0);
        }
        self.mem[target] = value;
    }
}
