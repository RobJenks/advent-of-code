use super::common;
use crate::common::grid::{Grid, GridDir};
use crate::day11::State::Unknown;
use std::fmt::{Display, Formatter};
use std::borrow::Borrow;

type States = Grid<State>;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    let (result, _) = evaluate_to_convergence(
        &parse_input(common::read_file("src/day11/problem-input.txt")), &step, Some(1e6 as usize));

    result.get_data().iter().filter(|x| **x == State::Occupied).count()
}

fn part2() -> usize {
    let (result, _) = evaluate_to_convergence(
        &parse_input(common::read_file("src/day11/problem-input.txt")), &step_with_visibility, Some(1e6 as usize));

    result.get_data().iter().filter(|x| **x == State::Occupied).count()
}

fn evaluate_to_convergence(grid: &States, step_fn: &dyn Fn(&States,&Vec<usize>,&Vec<usize>) -> States,
                           limit: Option<usize>) -> (States, usize) {

    let (ix_inner, ix_outer) = (grid.get_non_edge_indices(), grid.get_edge_indices());
    let mut result = grid.clone();
    let mut iterations = 0;

    loop {
        iterations += 1;

        let prev_data = result.get_data().clone();
        result = step_fn(&result, &ix_inner, &ix_outer);

        if result.get_data() == &prev_data { break }
        if limit.map_or(false, |lim| iterations > lim) { panic!("Exceeded iteration limit"); }
    }

    (result, iterations)
}

fn evaluate_steps(grid: &States, steps: usize) -> States {
    let (ix_inner, ix_outer) = (grid.get_non_edge_indices(), grid.get_edge_indices());
    (0..steps).fold(grid.clone(), |acc,_| step(&acc, &ix_inner, &ix_outer))
}

fn step(grid: &States, ix_inner: &Vec<usize>, ix_outer: &Vec<usize>) -> States {
    let mut new = grid.clone();

    // Not very DRY, but process edges and interior separately for efficiency; can do entire interior region
    // without bounds checks and this helps a lot for very large datasets & where system converges slowly
    ix_inner.iter().for_each(|&ix|
        match grid.get(ix) {
            State::Empty => if !grid.get_all_neighbours_unchecked(ix).contains(&&State::Occupied) { new.set(ix, State::Occupied ); },
            State::Occupied => if grid.get_all_neighbours_unchecked(ix).iter()
                .filter(|&x| x == &&State::Occupied).count() >= 4 { new.set(ix, State::Empty); },
            _ => ()
        }
    );

    ix_outer.iter().for_each(|&ix|
        match grid.get(ix) {
            State::Empty => if !grid.get_all_neighbours(ix).contains(&&State::Occupied) { new.set(ix, State::Occupied ); },
            State::Occupied => if grid.get_all_neighbours(ix).iter()
                .filter(|&x| x == &&State::Occupied).count() >= 4 { new.set(ix, State::Empty); },
            _ => ()
        }
    );

    new
}

fn step_with_visibility(grid: &States, _: &Vec<usize>, _: &Vec<usize>) -> States {
    let mut new = grid.clone();

    (0..grid.get_count())
        .map(|ix| (ix, grid.get(ix).clone()))
        .for_each(|(ix, state)| match state {
            State::Empty => if !get_visible(grid, ix).contains(&State::Occupied) { new.set(ix, State::Occupied); },
            State::Occupied => if get_visible(grid, ix).iter()
                .filter(|&s| s == &State::Occupied).count() >= 5 { new.set(ix, State::Empty); },
            _ => ()
        });

    new
}

fn get_visible(grid: &States, ix: usize) -> Vec<State> {
    let coord = grid.coords(ix);
    let mut visible = vec![];

    GridDir::all().iter()
        .for_each(|&dir| {
            let mut adj = Some(coord);
            loop {
                adj = grid.try_get_neighbour_coord(&adj.unwrap(), dir);
                match adj.map(|c| grid.get_at_coord(&c)) {
                    None => break,
                    Some(x) if x == &State::Occupied || x == &State::Empty => {
                        visible.push(x.clone());
                        break;
                    }
                    _ => ()
                };
            }
        });

    visible
}

fn parse_input(input: String) -> States {
    States::new_from_data(&input.lines()
        .map(|l| l.chars().map(|c| state_from_char(c)).collect::<Vec<State>>())
        .collect::<Vec<_>>())
}

#[derive(Clone, Debug, PartialOrd, PartialEq)]
#[repr(u8)]
pub enum State {
    Unknown,
    Floor,
    Empty,
    Occupied
}

impl Default for State {
    fn default() -> Self { Unknown }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match &self {
            &State::Floor => '.',
            &State::Empty => 'L',
            &State::Occupied => '#',
            _ => '?'
        })
    }
}

fn state_from_char(c: char) -> State {
    match c {
        '.' => State::Floor,
        'L' => State::Empty,
        '#' => State::Occupied,
        _ => State::Unknown
    }
}

#[cfg(test)]
mod tests {
    use super::common;
    use crate::day11::{parse_input, evaluate_to_convergence, State, evaluate_steps, step};


    #[test]
    pub fn test_basic_state_change() {
        let grid = parse_input(common::read_file("src/day11/test-input-01.txt"));
        assert_eq!(0, grid.get_data().iter().filter(|&x| x == &State::Occupied).count());
        assert_eq!(71, grid.get_data().iter().filter(|&x| x == &State::Empty).count());

        let step1 = evaluate_steps(&grid, 1);
        assert_eq!(71, step1.get_data().iter().filter(|&x| x == &State::Occupied).count());
        assert_eq!(0, step1.get_data().iter().filter(|&x| x == &State::Empty).count());

        let step2a = evaluate_steps(&grid, 2);
        let step2b = evaluate_steps(&step1, 1);
        assert_eq!(step2a.get_data(), step2b.get_data());
        assert_eq!(20, step2a.get_data().iter().filter(|&x| x == &State::Occupied).count());
        assert_eq!(51, step2a.get_data().iter().filter(|&x| x == &State::Empty).count());
    }

    #[test]
    pub fn test_convergence() {
        let grid = parse_input(common::read_file("src/day11/test-input-01.txt"));

        let (result, iterations) = evaluate_to_convergence(&grid, &step, Some(10));
        assert_eq!(6, iterations);
        assert_eq!(37, result.get_data().iter().filter(|&x| x == &State::Occupied).count());
    }
}