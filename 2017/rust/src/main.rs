
fn main() {
    let solutions = [
        day1::run, day2::run, day3::run, day4::run, day5::run,
        day6::run, day7::run, day8::run, day9::run, day10::run,
        day11::run, day12::run, day13::run, day14::run, day15::run
    ];

    for (i, &soln) in solutions.iter().enumerate() {
        println!("\nDay {}:\n", i+1);
        soln();
    }


}
