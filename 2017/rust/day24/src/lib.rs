pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    build_bridge(&mut parse_input(common::read_file("day24/input.txt")),0, &State::null(), &State::null(), &State::null(),
                 |state, best| State::new(std::cmp::max(state.strength, best.strength), 0))
        .strength
}


fn build_bridge(comp: &mut Vec<Comp>, port: u32, current_state: &State, new_state: &State, best: &State, heuristic: fn(&State, &State) -> State) -> State {
    let combined_state = State { strength: current_state.strength + new_state.strength, length: current_state.length + new_state.length };
    let mut new_best = heuristic(&best, &combined_state);

    let matches = comp.iter().enumerate()
        .filter(|(_, c)| (c.ports[0] == port || c.ports[1] == port) && !c.used)
        .map(|(i, _)| i)
        .collect::<Vec<usize>>();

    for i in matches {
        comp[i].used = true;
        let res = build_bridge(comp, if comp[i].ports[0] == port { comp[i].ports[1] } else { comp[i].ports[0] }, &combined_state,
                               &State { strength: comp[i].ports[0] + comp[i].ports[1], length: 1 }, &new_best, heuristic);
        new_best = heuristic(&new_best, &res);
        comp[i].used = false;
    }

    new_best
}


fn parse_input(input: String) -> Vec<Comp> {
    input.split("\n")
        .map(|s| s.split("/").map(|x| x.parse::<u32>().unwrap()).collect::<Vec<u32>>())
        .map(|v| Comp::new([v[0], v[1]]))
        .collect::<Vec<Comp>>()
}



#[derive(Debug)]
pub struct Comp {
    ports: [u32; 2],
    used: bool
}

#[derive(Debug)]
pub struct State {
    strength: u32,
    length: u32
}


impl Comp {
    pub fn new(p: [u32; 2]) -> Self { Self { ports: p, used: false } }
}
impl State {
    pub fn null() -> Self { Self { strength: 0, length: 0 } }
    pub fn new(strength: u32, length: u32) -> Self { Self { strength, length } }
}



#[cfg(test)]
mod tests {
    use super::{ State, parse_input, build_bridge };

    #[test]
    fn test_strength_heuristic() {
        let input = "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10".to_string();

        let res = build_bridge(&mut parse_input(input),0, &State::null(), &State::null(), &State::null(),
                     |state, best| State::new(std::cmp::max(state.strength, best.strength), 0));

        assert_eq!(res.strength, 31);
    }

}