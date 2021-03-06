
fn main() {
    let solutions = [
        day1::run,  day2::run,  day3::run,  day4::run,  day5::run,
        day6::run,  day7::run,  day8::run,  day9::run,  day10::run,
        day11::run, day12::run, day13::run, day14::run, day15::run,
        day16::run, day17::run, day18::run, day19::run, day20::run,
        day21::run, day22::run, day23::run, day24::run, day25::run
    ];

    solutions.iter().enumerate()
        .for_each(|(i, solution)| {
            println!("\nDay {}:\n", i + 1);
            solution();
        });

}
