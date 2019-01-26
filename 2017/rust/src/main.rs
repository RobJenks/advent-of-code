
fn main() {
    let solutions = [
        day1::run, day2::run, day3::run, day4::run, day5::run,
        day6::run, day7::run, day8::run
    ];

    for (i, &soln) in solutions.iter().enumerate() {
        println!("\nDay {}:\n", i+1);
        soln();
    }


}
