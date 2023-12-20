use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::iter::Sum;
use std::ops::Add;
use itertools::Itertools;

// --- System ---
#[derive(Clone)]
pub struct System {
    modules: Vec<Module>,
    indexed_modules: HashMap<String, usize>
}

impl System {
    pub fn new(modules: Vec<Module>) -> Self {
        Self {
            indexed_modules: modules.iter().map(|m| (m.id.clone(), m.ix)).collect::<HashMap<_, _>>(),
            modules
        }
    }

    pub fn push_button(&mut self) -> ExecutionResult {
        let broadcast = *self.indexed_modules.get("broadcaster").unwrap();
        let mut total_sent : [usize; 2] = [1, 0];

        let mut exec_queue = vec![(usize::MAX, broadcast, Pulse::Low)];
        while let Some((from, to, signal)) = exec_queue.pop() {
            if let Some(output) = self.modules[to].evaluate(signal, from) {
                let next_outputs = &self.modules[to].outputs;
                next_outputs.iter().for_each(|next| {
                    exec_queue.push((to, *next, output));
                });
                total_sent[output as usize] += next_outputs.len();
            }
        }
        ExecutionResult::new(total_sent[0], total_sent[1])
    }

    pub fn push_button_n_times(&mut self, n: usize) -> ExecutionResult {
        (0..n).map(|_| self.push_button())
            .sum()
    }
}
impl Display for System {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.modules.iter().join("\n"))
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ExecutionResult {
    pub low_pulse_count: usize,
    pub high_pulse_count: usize
}
impl ExecutionResult {
    pub fn new(low_pulse_count: usize, high_pulse_count: usize) -> Self {
        Self { low_pulse_count, high_pulse_count }
    }
    pub fn product(&self) -> usize {
        self.low_pulse_count * self.high_pulse_count
    }
}
impl Sum for ExecutionResult {
    fn sum<I: Iterator<Item=Self>>(iter: I) -> Self {
        iter.reduce(|acc, x| acc + x)
            .unwrap_or_else(|| ExecutionResult::new(0, 0))
    }
}
impl Add for ExecutionResult {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        ExecutionResult::new(
            self.low_pulse_count + rhs.low_pulse_count,
            self.high_pulse_count + rhs.high_pulse_count)
    }
}

// --- Pulse ---

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Pulse {
    Low = 0,
    High = 1
}
impl Display for Pulse {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if *self == Pulse::Low { "low" } else { "high "})
    }
}

// --- Module ---

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum ModuleType {
    NoOp, // Possible for undefined modules like 'output' which only act as a receiver
    Broadcast,
    FlipFlop,
    Conjunction
}
impl Display for ModuleType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}


pub struct Module {
    pub ix: usize,
    pub id: String,
    pub inputs: Vec<usize>,
    pub outputs: Vec<usize>,
    pub logic: Box<dyn ModuleLogic>
}

impl Module {
    pub fn new(ix: usize, id: String, inputs: Vec<usize>, outputs: Vec<usize>, logic: Box<dyn ModuleLogic>) -> Self {
        Self { ix, id, inputs, outputs, logic }
    }

    pub fn evaluate(&mut self, signal: Pulse, input_ix: usize) -> Option<Pulse> {
        self.logic.evaluate(signal, input_ix)
    }
}
impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({}, ix: {}, inputs: [{}], outputs: [{}])", self.id, self.logic, self.ix,
               self.inputs.iter().map(usize::to_string).join(", "),
               self.outputs.iter().map(usize::to_string).join(", "))
    }
}
impl Clone for Module {
    fn clone(&self) -> Self {
        Self { ix: self.ix, id: self.id.clone(), inputs: self.inputs.clone(), outputs: self.outputs.clone(),
               logic: build_logic(self.logic.get_type(), &self.inputs, &self.outputs) }
    }
}

// --- Module logic ---

pub trait ModuleLogic : Display {
    fn get_type(&self) -> ModuleType;
    fn evaluate(&mut self, signal: Pulse, input_ix: usize) -> Option<Pulse>;
}

pub fn build_logic(module_type: ModuleType, inputs: &Vec<usize>, _outputs: &Vec<usize>) -> Box<dyn ModuleLogic> {
    match module_type {
        ModuleType::Broadcast => Box::new(Broadcast::new()),
        ModuleType::FlipFlop => Box::new(FlipFlop::new()),
        ModuleType::Conjunction => Box::new(Conjunction::new(&inputs)),
        ModuleType::NoOp => Box::new(NoOp::new())
    }
}

// --- Module implementation: flip-flop ---

pub struct FlipFlop {
    on: bool
}

impl FlipFlop {
    pub fn new() -> Self {
        Self { on: false }
    }
}

impl ModuleLogic for FlipFlop {
    fn get_type(&self) -> ModuleType { ModuleType::FlipFlop }
    fn evaluate(&mut self, signal: Pulse, _: usize) -> Option<Pulse> {
        match signal {
            Pulse::Low => {
                self.on = !self.on;
                Some(if self.on { Pulse::High } else { Pulse::Low })
            },
            Pulse::High => None
        }
    }
}
impl Display for FlipFlop {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FlipFlop[{}]", if self.on { "on" } else { "off" })
    }
}

// --- Module implementation: conjunction ---

pub struct Conjunction {
    last_input: HashMap<usize, Pulse>
}

impl Conjunction {
    pub fn new(inputs: &Vec<usize>) -> Self {
        Self { last_input: inputs.iter().map(|ix| (*ix, Pulse::Low)).collect::<HashMap<_, _>>() }
    }
}

impl ModuleLogic for Conjunction {
    fn get_type(&self) -> ModuleType { ModuleType::Conjunction }
    fn evaluate(&mut self, signal: Pulse, input_ix: usize) -> Option<Pulse> {
        *self.last_input.get_mut(&input_ix).unwrap() = signal;

        if self.last_input.iter().all(|(_, p)| *p == Pulse::High) {
            Some(Pulse::Low)
        } else {
            Some(Pulse::High)
        }
    }
}
impl Display for Conjunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Conjunction[{}]", self.last_input.iter()
            .map(|(ix, state)| format!("{}={}", ix, state))
            .join(", "))
    }
}

// --- Module implementation: broadcast ---

pub struct Broadcast {
}

impl Broadcast {
    pub fn new() -> Self { Self { } }
}

impl ModuleLogic for Broadcast {
    fn get_type(&self) -> ModuleType { ModuleType::Broadcast }
    fn evaluate(&mut self, signal: Pulse, _: usize) -> Option<Pulse> {
        Some(signal)
    }
}
impl Display for Broadcast {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Broadcast")
    }
}

// --- Module implementation: no-op ---

pub struct NoOp { }

impl NoOp {
    pub fn new() -> Self { Self { } }
}

impl ModuleLogic for NoOp {
    fn get_type(&self) -> ModuleType { ModuleType::NoOp }
    fn evaluate(&mut self, _: Pulse, _: usize) -> Option<Pulse> { None }
}
impl Display for NoOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NoOp")
    }
}