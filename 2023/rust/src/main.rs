extern crate itertools;
#[macro_use] extern crate scan_fmt;

mod common;
mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
mod day22;
mod day23;

fn main() {
    let solutions = [
        day1::run, day2::run, day3::run, day4::run, day5::run,
        day6::run, day7::run, day8::run, day9::run, day10::run,
        day11::run, day12::run, day13::run, day14::run, day15::run,
        day16::run, day17::run, day18::run, day19::run, day20::run,
        day21::run, day22::run, day23::run
    ];

    solutions.iter().enumerate().skip(22)
        .for_each(|(i, solution)| {
            println!("\nDay {}:\n", i + 1);
            solution();
        });
}
