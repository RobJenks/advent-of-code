extern crate itertools;

mod common;
mod day1;

fn main() {
    let solutions = [
        day1::run
    ];

    solutions.iter().enumerate()
        .for_each(|(i, solution)| {
            println!("\nDay {}:\n", i + 1);
            solution();
        });
}
