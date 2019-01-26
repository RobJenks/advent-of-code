use std::collections::HashMap;

type Registers = HashMap<String, i32>;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> i32 {
    *process_input(common::read_file("day8/input.txt"), |_,_| 0).0
        .values().max().unwrap_or_else(|| panic!("Failure"))
}

fn part2() -> i32 {
    process_input(common::read_file("day8/input.txt"), |acc,x| std::cmp::max(acc, x)).1
}

fn process_input(input: String, acc_fn: fn(i32,i32) -> i32) -> (Registers, i32) {
    let mut reg = Registers::new();
    let mut acc : i32 = 0;

    input.split("\n")
        .map(|x| x.split_whitespace().collect::<Vec<&str>>())
        .for_each(|vec| {
            if check_cond(&mut reg, &vec[4..7]) {
                apply_op(&mut reg, &vec[0..3]);
                acc = acc_fn(acc, get_reg(&mut reg, vec[0]));
            }
        });
    (reg, acc)
}

fn get_reg(all: &mut Registers, id: &str) -> i32 {
    if !all.contains_key(id) { all.insert(id.to_string(), 0); }
    *all.get(id).unwrap()
}

fn get_reg_mut<'a, 'b>(all: &'a mut Registers, id: &'b str) -> &'a mut i32 {
    if !all.contains_key(id) { all.insert(id.to_string(), 0); }
    all.get_mut(id).unwrap()
}

fn check_cond(all: &mut Registers, cond: &[&str]) -> bool {
    let val = get_reg(all, cond[0]);
    match cond {
        [_, op, x] if *op == "==" => val == x.parse().unwrap(),
        [_, op, x] if *op == "!=" => val != x.parse().unwrap(),
        [_, op, x] if *op == "<"  => val <  x.parse().unwrap(),
        [_, op, x] if *op == "<=" => val <= x.parse().unwrap(),
        [_, op, x] if *op == ">"  => val >  x.parse().unwrap(),
        [_, op, x] if *op == ">=" => val >= x.parse().unwrap(),
        _ => panic!("Unknown operation")
    }
}

fn apply_op(all: &mut Registers, op: &[&str]) {
    let val = get_reg_mut(all, op[0]);
    *val += match op {
        [_, op, val] if *op == "inc" => val.parse::<i32>().unwrap(),
        [_, op, val] if *op == "dec" => -(val.parse::<i32>().unwrap()),
        _ => panic!("Invalid operation")
    }
}
