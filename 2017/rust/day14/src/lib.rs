use common::hash::{calculate_ascii_hash_binary};

const INPUT : &str = "hfdlxzhv";

pub fn run() {
    println!("Part 1 result: {}", part1(INPUT.to_string()));
    println!("Part 2 result: {}", part2(INPUT.to_string()));
}

fn part1(input: String) -> usize {
    (0..128)
        .map(|x|calculate_ascii_hash_binary(format!("{}-{}", input, x)))
        .map(|x| x.chars().filter(|c| *c == '1').count())
        .sum()
}

fn part2(input: String) -> u32 {
    let data = (0..128)
        .map(|x|calculate_ascii_hash_binary(format!("{}-{}", input, x)))
        .map(|x| x.chars().map(|c| c == '1').collect::<Vec<bool>>())
        .collect::<Vec<Vec<bool>>>();

    let mut group = 0u32;
    let mut groups = vec![[0u32; 128]; 128];

    for y in 0..128 { for x in 0..128 {
        if !data[x][y] || groups[x][y] != 0 { continue; }

        group += 1;
        groups[x][y] = group;

        let mut eval = adj(x, y);
        while !eval.is_empty() {
            let el = eval.pop().unwrap();
            if !data[el.0][el.1] || groups[el.0][el.1] != 0 { continue; }

            groups[el.0][el.1] = group;
            eval.append(&mut adj(el.0, el.1));
        }
    }}

    group
}


fn adj(x: usize, y: usize) -> Vec<(usize,usize)> {
    let mut res = vec![];
    if x > 0 { res.push((x-1, y)); }
    if y > 0 { res.push((x, y-1)); }
    if x < 127 { res.push((x+1, y)); }
    if y < 127 { res.push((x, y+1)); }

    res
}

#[cfg(test)]
mod tests {
    use super::{calculate_ascii_hash_binary};

    #[test]
    fn test_hash() {
        let expected = vec!["11010100", "01010101", "00001010", "10101101", "01101000", "11001001", "01000100", "11010110"];
        let actual = (0..8).map(|x| calculate_ascii_hash_binary(format!("flqrgnkx-{}", x))).collect::<Vec<String>>();

        expected.iter().zip(actual.iter()).for_each(|(exp, act)| assert_eq!(*exp, &act[0..8]));
    }
}
