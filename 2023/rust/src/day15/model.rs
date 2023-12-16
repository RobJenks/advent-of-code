use itertools::Itertools;
use crate::day15::hash;

pub struct Array {
    pub cells: [Cell; 256]
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Cell {
    pub lenses: Vec<Lens>
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Lens {
    pub label: String,
    pub focal_length: u32
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Instruction {
    Add(String, u32),
    Remove(String)
}

impl Array {
    pub fn new() -> Self {
        Self { cells: std::array::from_fn(|_| Cell::new()) }
    }

    pub fn process_instruction(&mut self, inst: &Instruction) {
        match inst {
            Instruction::Add(label, focal_length) => self.add_to_cell(label, *focal_length),
            Instruction::Remove(label) => self.remove_from_cell(label)
        };
    }

    fn add_to_cell(&mut self, label: &String, focal_length: u32) {
        let cell = &mut self.cells[hash(label) as usize];
        if let Some((_, lens)) = cell.lenses.iter_mut().find_position(|x| &x.label == label) {
            lens.focal_length = focal_length;
        }
        else {
            cell.lenses.push(Lens::new(label.clone(), focal_length));
        }
    }

    fn remove_from_cell(&mut self, label: &String) {
        let cell = &mut self.cells[hash(label) as usize];
        if let Some((ix, _)) = cell.lenses.iter().find_position(|x| &x.label == label) {
            cell.lenses.remove(ix);
        }
    }

    pub fn get_total_focusing_power(&self) -> usize {
        self.cells.iter().enumerate()
            .map(|(ix, cell)| cell.get_total_focusing_power(ix))
            .sum()
    }

    pub fn _to_string(&self) -> String {
        self.cells.iter().enumerate()
            .filter(|&(_, cell)| !cell.lenses.is_empty())
            .map(|(ix, cell)| format!("Box {} {}", ix+1, cell._to_string()))
            .join("\n")
    }
}

impl Cell {
    pub fn new() -> Self {
        Self { lenses: Vec::new() }
    }

    pub fn get_total_focusing_power(&self, cell_number: usize) -> usize {
        self.lenses.iter().enumerate()
            .map(|(ix, lens)| (cell_number + 1) * (ix + 1) * lens.focal_length as usize)
            .sum()
    }

    pub fn _to_string(&self) -> String {
        self.lenses.iter()
            .map(|lens| format!("[{} {}]", lens.label, lens.focal_length))
            .join(" ")
    }
}

impl Lens {
    pub fn new(label: String, focal_length: u32) -> Self {
        Self { label, focal_length }
    }
}

impl Instruction {
    pub fn new(str: &String) -> Self {
        if str.contains("=") {
            let comp = str.split("=").collect_tuple::<(&str, &str)>().unwrap_or_else(|| panic!("Invalid assignment instruction"));
            Instruction::Add(comp.0.to_string(), comp.1.parse::<u32>().unwrap_or_else(|_| panic!("Invalid focal length")))
        }
        else {
            Instruction::Remove(str[0..str.len() - 1].to_string())
        }
    }
}