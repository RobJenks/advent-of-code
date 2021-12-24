extern crate itertools;

mod common;
mod day1;
mod day2;
mod day3;
mod day4;
mod day5;

fn main() {
    let solutions = [
        day1::run, day2::run, day3::run, day4::run, day5::run
    ];

    solutions.iter().enumerate()
        .for_each(|(i, solution)| {
            println!("\nDay {}:\n", i + 1);
            solution();
        });
}

