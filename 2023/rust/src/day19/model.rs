use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use itertools::Itertools;

// --- Field ---

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Field {
    X = 0,
    M = 1,
    A = 2,
    S = 3
}

impl Field {
    pub fn from_char(c: char) -> Self {
        match c {
            'x' => Self::X,
            'm' => Self::M,
            'a' => Self::A,
            's' => Self::S,
            _ => panic!("Unrecognized field '{}'", c)
        }
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

// --- Comparison ---

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Comparison {
    None,
    LessThan(Field, usize),
    GreaterThan(Field, usize)
}

impl Comparison {
    pub fn inequality_from_str(str: &str) -> Self {
        if let Some((field, value)) = str.split_once("<") {
            Comparison::LessThan(Field::from_char(field.chars().next().unwrap_or_else(|| panic!("Missing field name"))),
                                 value.parse::<usize>().unwrap_or_else(|_| panic!("Invalid inequality value")))
        }
        else if let Some((field, value)) = str.split_once(">") {
            Comparison::GreaterThan(Field::from_char(field.chars().next().unwrap_or_else(|| panic!("Missing field name"))),
                                    value.parse::<usize>().unwrap_or_else(|_| panic!("Invalid inequality value")))
        }
        else {
            panic!("Invalid inequality")
        }
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Comparison::None => write!(f, ""),
            Comparison::LessThan(field, value) => write!(f, "<{}", value),
            Comparison::GreaterThan(field, value) => write!(f, ">{}", value)
        }
    }
}

// --- Action ---

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Action {
    PassTo(String),
    Accept,
    Reject
}

impl Action {
    pub fn from_str(str: &str) -> Self {
        match str {
            "A" => Self::Accept,
            "R" => Self::Reject,
            wf @ _ => Self::PassTo(wf.to_string()),
        }
    }
}

impl Display for Action {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::PassTo(wf) => write!(f, "{}", wf),
            Action::Accept => write!(f, "A"),
            Action::Reject => write!(f, "R")
        }
    }
}

// --- Part ---

#[derive(Clone)]
pub struct Part {
    data: [usize; 4]
}

impl Part {
    pub fn new(x: usize, m: usize, a: usize, s: usize) -> Self {
        Self { data: [x, m, a, s] }
    }
    pub fn x(&self) -> usize { self.data[0] }
    pub fn m(&self) -> usize { self.data[1] }
    pub fn a(&self) -> usize { self.data[2] }
    pub fn s(&self) -> usize { self.data[3] }
}

impl Display for Part {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ x={}, m={}, a={}, s={} }}", self.x(), self.m(), self.a(), self.s())
    }
}

// --- Rule ---

#[derive(Clone)]
pub struct Rule {
    criteria: Comparison,
    action: Action
}

impl Rule {
    pub fn new(criteria: Comparison, action: Action) -> Self {
        Self { criteria, action }
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.criteria {
            Comparison::None => write!(f, "{}", self.action),
            _ => write!(f, "{}:{}", self.criteria, self.action)
        }
    }
}

// --- Workflow ---

#[derive(Clone)]
pub struct Workflow {
    name: String,
    rules: Vec<Rule>
}

impl Workflow {
    pub fn new(name: String, rules: Vec<Rule>) -> Self {
        Self { name, rules }
    }
}

impl Display for Workflow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{ {} }}", self.name, self.rules.iter()
            .map(|rule| rule.to_string())
            .join(", "))
    }
}

// --- Model ---

#[derive(Clone)]
pub struct Model {
    workflows: HashMap<String, Workflow>,
    parts: Vec<Part>,
    accepted: Vec<usize>,
    rejected: Vec<usize>
}

impl Model {
    pub fn new(workflows: Vec<Workflow>, parts: Vec<Part>) -> Self {
        Self { workflows: workflows.iter().map(|wf| (wf.name.clone(), wf.to_owned())).collect::<HashMap<_, _>>(),
               parts, accepted: Vec::new(), rejected: Vec::new() }
    }

    pub fn evaluate(&mut self) {
        let input_wf = self.workflows.get("in").unwrap_or_else(|| panic!("No input workflow"));

        self.parts.iter().enumerate()
            .map(|(ix, part)| (ix, self.evaluate_part(part, input_wf)))
            .for_each(|(ix, is_accepted)| match is_accepted {
                true => self.accepted.push(ix),
                false => self.rejected.push(ix),
            });
    }

    fn evaluate_part(&self, part: &Part, input_wf: &Workflow) -> bool {
        
    }
}
