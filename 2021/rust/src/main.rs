#![feature(iter_intersperse)]
extern crate itertools;

mod common;
mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;

fn main() {
    let solutions = [
        day1::run, day2::run, day3::run, day4::run, day5::run,
        day6::run, day7::run
    ];

    solutions.iter().enumerate()
        .for_each(|(i, solution)| {
            println!("\nDay {}:\n", i + 1);
            solution();
        });
}

