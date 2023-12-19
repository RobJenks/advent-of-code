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
            panic!("Invalid inequality '{}'", str)
        }
    }

    pub fn matches(&self, part: &Part) -> bool {
        match self {
            Comparison::LessThan(field, val) => part.data[*field as usize] < *val,
            Comparison::GreaterThan(field, val) => part.data[*field as usize] > *val,
            Comparison::None => true
        }
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Comparison::None => write!(f, ""),
            Comparison::LessThan(field, value) => write!(f, "{}<{}", field, value),
            Comparison::GreaterThan(field, value) => write!(f, "{}>{}", field, value)
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

    pub fn sum_components(&self) -> usize {
        self.data.iter().sum()
    }
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

    pub fn evaluate(&self, part: &Part) -> &Action {
        self.rules.iter()
            .find(|rule| rule.criteria.matches(part))
            .map(|rule| &rule.action)
            .unwrap_or_else(|| panic!("Failed to match rule conditions"))
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
    pub workflows: HashMap<String, Workflow>,
    pub parts: Vec<Part>,
    pub accepted: Vec<usize>,
    pub rejected: Vec<usize>
}

impl Model {
    pub fn new(workflows: Vec<Workflow>, parts: Vec<Part>) -> Self {
        Self { workflows: workflows.iter().map(|wf| (wf.name.clone(), wf.to_owned())).collect::<HashMap<_, _>>(),
               parts, accepted: Vec::new(), rejected: Vec::new() }
    }

    pub fn evaluate(&mut self) {
        let input_wf = self.workflows.get("in").unwrap_or_else(|| panic!("No input workflow"));

        for (ix, part) in self.parts.iter().enumerate() {
            match self.evaluate_part(part, input_wf) {
                true => self.accepted.push(ix),
                false => self.rejected.push(ix),
            }
        }
    }

    fn evaluate_part(&self, part: &Part, input_wf: &Workflow) -> bool {
        let mut wf = input_wf;
        loop {
            match wf.evaluate(part) {
                Action::Accept => break true,
                Action::Reject => break false,
                Action::PassTo(next) => wf = self.workflows.get(next).unwrap_or_else(|| panic!("Missing workflow '{}' in chain", next))
            }
        }
    }
    pub fn get_accepted_parts(&self) -> impl Iterator<Item = &Part> {
        self.accepted.iter()
            .map(|ix| &self.parts[*ix])
    }

    pub fn determine_accepted_domain(&self, base_domain: Domain) -> usize {
        self.subdivide_domain(base_domain, "in")
    }

    fn subdivide_domain(&self, domain: Domain, workflow: &str) -> usize {
        if workflow == "R" ||  domain.get_domain_size() == 0 { return 0 }
        if workflow == "A" { return domain.get_domain_size() }

        let wf= self.workflows.get(workflow)
            .unwrap_or_else(|| panic!("Could not locate next workflow '{}' in domain calculation", workflow));

        let mut total = 0usize;
        let mut working_domain = domain.clone();

        for rule in &wf.rules {
            let (a, b) = working_domain.split(&rule.criteria);
            match &rule.action {
                Action::Accept => total += self.subdivide_domain(a.clone(), "A"),
                Action::Reject => total += self.subdivide_domain(a.clone(), "R"),
                Action::PassTo(next) => total += self.subdivide_domain(a.clone(), next)
            }

             working_domain = b;    // Process remaining rules on remaining subdomain
        }
        total
    }
}

// --- Domain/Range ---

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Domain {
    pub ranges: [Range; 4]
}

impl Domain {
    pub fn new(start: usize, end: usize) -> Self {
        Self { ranges: [Range::new(start, end); 4] }
    }

    pub fn zero() -> Self {
        Self::new(0, 0)
    }

    pub fn clone_with(&self, field: Field, range: Range) -> Self {
        let mut cloned = self.clone();
        cloned.ranges[field as usize] = range;
        cloned
    }

    pub fn get_domain_size(&self) -> usize {
        self.ranges.iter().map(Range::get_size).product()
    }

    pub fn split(&self, criteria: &Comparison) -> (Domain, Domain) {
        match *criteria {
            Comparison::LessThan(field, value) => self.split_lt(field, value),
            Comparison::GreaterThan(field, value) => self.split_gt(field, value),
            Comparison::None => (self.clone(), Domain::zero()) // i.e. [all][none]
        }
    }

    fn split_lt(&self, field: Field, value: usize) -> (Domain, Domain) {
        let current = &self.ranges[field as usize];

        if current.start >= value {
            (self.clone_with(field, Range::empty()), self.clone())    // Entire range is > value, so return [][all]
        }
        else if current.end <= value {
            (self.clone(), self.clone_with(field, Range::empty()))    // Entire range is < value, so return [all][]
        }
        else {
            (
                self.clone_with(field, Range::new(current.start, value)),
                self.clone_with(field, Range::new(value, current.end))
            )
        }
    }

    fn split_gt(&self, field: Field, value: usize) -> (Domain, Domain) {
        let current = &self.ranges[field as usize];
        if (current.end - 1) <= (value + 1) {
            (self.clone_with(field, Range::empty()), self.clone())    // Entire range is < value, so return [][all]
        }
        else if current.start > value {
            (self.clone(), self.clone_with(field, Range::empty()))    // Entire range is > value, so return [all][]
        }
        else {
            (
                self.clone_with(field, Range::new(value + 1, current.end)),
                self.clone_with(field, Range::new(current.start, value + 1))
            )
        }
    }

    #[cfg(test)]
    pub fn to_tuples(&self) -> [(usize, usize); 4] {
        [self.ranges[0].to_tuple(), self.ranges[1].to_tuple(), self.ranges[2].to_tuple(), self.ranges[3].to_tuple()]
    }
}
impl Display for Domain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "x:[{}), m:[{}), a:[{}), s:[{})", self.ranges[0], self.ranges[1], self.ranges[2], self.ranges[3])
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Range {
    pub start: usize,   // inclusive
    pub end: usize      // exclusive
}

impl Range {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }

    pub fn get_size(&self) -> usize {
        if self.start > self.end { 0 } else { self.end - self.start }
    }

    #[cfg(test)]
    pub fn to_tuple(&self) -> (usize, usize) {
        (self.start, self.end)
    }
}
impl Display for Range {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}
