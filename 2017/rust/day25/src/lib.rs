pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    let (mut tm, checksum) = parse_input(common::read_file("day25/input.txt"), 10_000);
    tm.by_ref().take(checksum).last().unwrap();

    tm.get_tape().iter().filter(|x| **x == 1).count()
}


#[derive(Debug)]
pub struct TuringMachine {
    states: Vec<State>,     // All possible TM states
    state: usize,           // Index of current state
    ptr: usize,             // Pointer into TM tape
    tape: Vec<u8>           // 'Infinite' tape
}

#[derive(Debug)]
pub struct State {
    name: char,
    write: [u8; 2],         // Value to write when in this state, when current value is {0, 1}
    mv: [isize; 2],            // Translation following the write, when current value is {0, 1}
    next: [usize; 2],       // Next state transition, when current value is {0, 1}
}

impl TuringMachine {
    pub fn new(states: Vec<State>, initial_state: usize, tape_length: usize) -> Self {
        Self { states, state: initial_state, tape: vec![0u8; tape_length], ptr: tape_length / 2 }
    }
    pub fn get_tape(&self) -> &Vec<u8> { &self.tape }
}

impl State {
    pub fn new(name: char, write: [u8; 2], mv: [isize; 2], next: [usize; 2]) -> Self {
        Self { name, write, mv, next}
    }
}

impl Iterator for TuringMachine {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.tape[self.ptr] as usize;
        let state = &self.states[self.state];

        self.tape[self.ptr] = state.write[val];
        self.ptr = (self.ptr as isize + state.mv[val]) as usize;
        self.state = state.next[val];

        Some(self.state)
    }
}



fn parse_input(input: String, tape_length: usize) -> (TuringMachine, usize) {
    let t = input.split("\n")
        .map(|s| s.split_whitespace().map(|x| x.trim()).collect::<Vec<&str>>())
        .collect::<Vec<Vec<&str>>>();

    // Assume the very specific structure of the problem input to avoid too much text parsing
    verify(t[0][0], "Begin");
    let initial = t[0][3].chars().nth(0).unwrap() as usize - 'A' as usize;

    verify(t[1][0], "Perform");
    let checksum = t[1][5].parse::<usize>().unwrap();

    let mut states : Vec<State> = vec![];
    for i in 3..t.len() as usize {
        if t[i].is_empty() { continue }
        if t[i][0] == "In" {                // New state definition
            let name = t[i][2].chars().nth(0).unwrap();
            let write =
                [t[i+2][4].chars().nth(0).unwrap().to_digit(10).unwrap() as u8,
                 t[i+6][4].chars().nth(0).unwrap().to_digit(10).unwrap() as u8];
            let mv =
                [if t[i+3][6] == "left." { -1isize } else { 1isize },
                 if t[i+7][6] == "left." { -1isize } else { 1isize }];
            let next =
                [t[i+4][4].chars().nth(0).unwrap() as usize - 'A' as usize,
                 t[i+8][4].chars().nth(0).unwrap() as usize - 'A' as usize];

            states.push(State::new(name, write, mv, next));
        }
    }

    (TuringMachine::new(states, initial,tape_length), checksum)
}

fn verify(input: &str, exp: &str) {
    if input != exp { panic!("Unexpected input data; expected \"{}\" but encountered \"{}\"", exp, input); }
}



#[cfg(test)]
mod tests {
    use super::{parse_input};

    #[test]
    fn test_transitions() {
        let (mut tm, checksum) = parse_input(common::read_file("tests.txt"), 25);

        let final_state = tm.by_ref().take(checksum).last().unwrap();
        assert_eq!(final_state, 0usize);
        assert_eq!(tm.get_tape().iter().filter(|x| **x == 1).count(), 3);
    }

}