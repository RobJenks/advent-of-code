
fn main() {
    let solutions = [ day1::run ];

    for (i, &soln) in solutions.iter().enumerate() {
        println!("\nDay {}:\n", i+1);
        soln();
    }


}
