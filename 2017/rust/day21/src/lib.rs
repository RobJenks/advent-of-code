use std::collections::hash_map::HashMap;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    Image::new(parse_rules(common::read_file("day21/input.txt")))
        .take(5)
        .last().unwrap()
        .chars()
        .filter(|c| *c == '#')
        .count()
}

fn part2() -> usize {
    Image::new(parse_rules(common::read_file("day21/input.txt")))
        .take(18)
        .last().unwrap()
        .chars()
        .filter(|c| *c == '#')
        .count()
}


type Rules = HashMap<String, String>;
type ImageArray = Vec<Vec<char>>;

pub struct Image {
    data: String,
    rules: Rules
}

impl Image {
    const INITIAL_IMAGE : &'static str = ".#./..#/###";
    fn new(rules: Rules) -> Self { Self { data: Image::INITIAL_IMAGE.to_string(), rules } }

    pub fn to_array(data: &String) -> ImageArray {
        data.split("/")
            .map(|s| s.chars().collect::<Vec<char>>())
            .collect::<ImageArray>()
    }

    pub fn to_string(ia: &ImageArray) -> String {
        let mut res = ia.iter()
            .map(|v| v.iter().collect::<String>() + "/")
            .collect::<String>();
        res.truncate(res.len() - 1);
        res
    }

    fn generate_subarrays(&self) -> Vec<((usize,usize), ImageArray)> {
        let a = Self::to_array(&self.data);
        let size = a.len();
        let mut sub = vec![];

        if size % 2 == 0 {  // 2x2 subarrays
            (0..size/2).map(|x| x * 2).for_each(|x| {
                (0..size/2).map(|y| y * 2).for_each(|y| {
                    sub.push(((x/2,y/2), vec![vec![a[x][y], a[x+1][y]], vec![a[x][y+1], a[x+1][y+1]]]));
                })
            })
        }
        else {              // 3x3 subarrays
            (0..size/3).map(|x| x * 3).for_each(|x| {
                (0..size/3).map(|y| y * 3).for_each(|y| {
                    sub.push(((x/3,y/3), vec![vec![a[x+0][y+0], a[x+1][y+0], a[x+2][y+0]],
                                                    vec![a[x+0][y+1], a[x+1][y+1], a[x+2][y+1]],
                                                    vec![a[x+0][y+2], a[x+1][y+2], a[x+2][y+2]]]));
                })
            })
        }

        sub
    }

    fn combine_subarrays(sub: Vec<((usize,usize), ImageArray)>) -> ImageArray {
        let size = (sub.len() as f64).sqrt() as usize * sub[0].1.len();
        let mut all = vec![vec!['X'; size]; size];

        sub.iter().for_each(|(p, vv)| {
            for x in 0..vv.len() { for y in 0..vv.len() {
                all[x+(p.0*vv.len())][y+(p.1*vv.len())] = vv[x][y];
            }}
        });

        all
    }

    fn generate_combinations(ia: &ImageArray) -> Vec<ImageArray> {
        let bound = ia.len() - 1;

        let mut com : Vec<ImageArray> = vec![ia.clone(), ia.clone(), ia.clone(), ia.clone()];
        for x in 0..=bound { for y in 0..=bound {
            com[1][x][y] = com[0][bound-x][y];
            com[2][x][y] = com[0][x][bound-y];
            com[3][x][y] = com[0][bound-x][bound-y];
        }}

        // Generate four rotations for each of the base configurations -> 16 combinations in total
        let rot = |base: &ImageArray| {
            let mut res = vec![vec!['X'; bound+1]; bound+1];
            for x in 0..=bound { for y in 0..=bound {
                res[y][bound-x] = base[x][y];
            }}
            res
        };

        for _ in 0..3 as usize {
            let src = com.len() - 4;
            com.push(rot(&com[src]));
        }

        com
    }

    fn _match_rules(&self, data: &String) -> String {
        Self::to_string(&self.match_rules_array(&Image::to_array(data)))
    }

    fn match_rules_array(&self, data: &ImageArray) -> ImageArray {
        let matches = Self::generate_combinations(data).iter()
            .map(|x| Self::to_string(x))
            .map(|x| self.rules.get(&x))
            .filter(|x| x.is_some())
            .map(|x| Self::to_array(x.unwrap()))
            .collect::<Vec<ImageArray>>();

        if matches.is_empty() { panic!("No rule match found"); }
        matches[0].clone()
    }

}

impl Iterator for Image {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        let sub = self.generate_subarrays().iter()
            .map(|(p, x)| (*p, self.match_rules_array(x)))
            .collect();

        self.data = Self::to_string(&Self::combine_subarrays(sub));
        Some(self.data.clone())
    }
}


fn parse_rules(input: String) -> HashMap<String, String> {
    input.split("\n")
        .map(|s| s.split(" => ").collect::<Vec<&str>>())
        .map(|v| (v[0].trim().to_string(), v[1].trim().to_string()))
        .collect::<Rules>()
}

#[cfg(test)]
mod tests {
    use super::{Image, ImageArray, parse_rules};

    #[test]
    fn test_matching() {
        let expected = vec!["#..#/..../..../#..#".to_string(),
                                        "##.##./#..#../....../##.##./#..#../......".to_string()];

        let rules = parse_rules(common::read_file("test-matching.txt"));
        let mut image = Image::new(rules);
        assert_eq!(image.data, ".#./..#/###".to_string());

        for i in 0..2 {
            assert_eq!(image.next().unwrap(), expected[i]);
        }
    }

}