use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    let mut state = parse_input(common::read_file("src/day4/problem-input.txt"));
    let result = eval_to_winner(&mut state);

    result.0.unmarked_sum() * result.1
}


fn parse_input(input: String) -> State {
    let seq = input.lines().next().expect("Missing data")
        .split(',')
        .map(|c| c.parse::<u32>().expect("Non-numeric sequence"))
        .collect::<Vec<_>>();

    let boards = input.lines().skip(2).collect::<Vec<_>>()
        .chunks(6)
        .map(parse_board)
        .collect::<Vec<_>>();

    State::new(seq, boards)
}

fn parse_board(chunk: &[&str]) -> Board {
    let mut values = [[0u32; 5]; 5];
    chunk.iter()
        .enumerate()
        .take(5)
        .for_each(|(row_ix, &row)| row.split_whitespace()
            .enumerate()
            .map(|(col_ix, s)| (col_ix, s.parse::<u32>().expect("Invalid board value")))
            .for_each(|(col_ix, v)| values[row_ix][col_ix] = v));

    Board::new(values)
}

// Returns winning board and last number called
fn eval_to_winner(state: &mut State) -> (Board, u32) {
    let mut seq = state.seq.iter();
    loop {
        let v = seq.next().expect("No solution");
        state.boards.iter_mut().for_each(|b| b.record_number(*v));
        if let Some(board) = state.get_winner() {
            break (board.clone(), *v);
        }
    }
}

#[derive(Debug)]
struct State {
    seq: Vec<u32>,
    boards: Vec<Board>
}

#[derive(Clone, Debug)]
struct Board {
    rows: [[Value; 5]; 5]
}

#[derive(Clone, Debug)]
struct Value {
    value: u32,
    marked: bool
}

impl State {
    pub fn new(seq: Vec<u32>, boards: Vec<Board>) -> Self {
        Self { seq, boards }
    }

    pub fn get_winner(&self) -> Option<&Board> {
        self.boards.iter()
            .find(|&board| board.is_winner())
    }
}

impl Board {
    pub fn new(rows: [[u32; 5]; 5]) -> Self {
        Self { rows: rows.map(|row| row.map(|v| Value::new(v))) }
    }

    pub fn record_number(&mut self, num: u32) {
        self.rows.iter_mut().for_each(
            |row| row.iter_mut().for_each(
                |v| if v.value == num { v.marked = true; }));
    }

    pub fn is_winner(&self) -> bool {
        self.rows.iter()        // Any row
            .any(|row| row.iter().all(|el| el.marked))

        ||

        self.rows.iter()        // Any col
            .fold(vec![true; 5], |acc, row|
                acc.iter().enumerate().map(|(i, b)| *b & row[i].marked).collect::<Vec<_>>()
            )
            .iter().any(|v| *v)
    }

    pub fn unmarked_sum(&self) -> u32 {
        self.rows.iter()
            .fold(0u32, |acc, row| {let a:u32 = row.iter()
                .filter(|&v| !v.marked)
                .map(|x| x.value)
                .sum();acc+a})
    }

    #[allow(dead_code)]
    pub fn render(&self) -> String {
        self.rows.iter()
            .map(|row| row.iter()
                .map(|v| format!("  {}{}", v.value.to_string(), if v.marked { "* " } else { "  " }))
                .collect::<String>())
            .intersperse(String::from("\n"))
            .collect::<String>()
    }
}

impl Value {
    pub fn new(value: u32) -> Self {
        Self { value, marked: false }
    }
}

#[cfg(test)]
mod test {
    use super::common;
    use crate::day4::{parse_input, eval_to_winner};

    #[test]
    fn test_evaluation() {
        let mut state = parse_input(common::read_file("src/day4/test-input.txt"));
        let result = eval_to_winner(&mut state);

        assert_eq!(result.0.unmarked_sum(), 188);
        assert_eq!(result.1, 24);
    }
}