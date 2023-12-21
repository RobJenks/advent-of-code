use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};
use std::iter::Sum;
use std::ops::Add;
use itertools::Itertools;

// --- System ---
#[derive(Clone)]
pub struct System {
    pub modules: Vec<Module>,
    pub indexed_modules: HashMap<String, usize>
}

impl System {
    pub fn new(modules: Vec<Module>) -> Self {
        Self {
            indexed_modules: modules.iter().map(|m| (m.id.clone(), m.ix)).collect::<HashMap<_, _>>(),
            modules
        }
    }

    pub fn eval(&mut self) -> ExecutionResult {
        self.eval_with_stop_condition(&None::<fn(usize, Pulse) -> bool>)
    }

    pub fn eval_with_stop_condition(&mut self, stop_condition: &Option<impl Fn(usize, Pulse) -> bool>) -> ExecutionResult {
        let broadcast = *self.indexed_modules.get("broadcaster").unwrap();
        let mut total_sent : [usize; 2] = [1, 0];
        let mut hit_stop_condition = false;

        let mut exec_queue = VecDeque::new();
        exec_queue.push_back((usize::MAX, broadcast, Pulse::Low));

        while let Some((from, to, signal)) = exec_queue.pop_front() {
            if let Some(output) = self.modules[to].evaluate(signal, from) {
                let next_outputs = &self.modules[to].outputs;
                next_outputs.iter().for_each(|next| {
                    exec_queue.push_back((to, *next, output));
                });
                total_sent[output as usize] += next_outputs.len();

                if stop_condition.as_ref().map(|f| f(to, output)).unwrap_or_else(|| false) {
                    hit_stop_condition = true;
                    break;
                }
            }
        }
        ExecutionResult::new(1, total_sent[0], total_sent[1], hit_stop_condition)
    }

    pub fn eval_n(&mut self, n: usize) -> ExecutionResult {
        (0..n).map(|_| self.eval())
            .sum()
    }

    pub fn _cycles_until_receiver_triggered(&mut self, receiver: &str) -> usize {
        let receiver_ix = *self.indexed_modules.get(receiver).unwrap_or_else(|| panic!("Receiver does not exist"));
        let mut cycles = 0usize;

        loop {
            cycles += 1;
            self.eval();

            if self.modules[receiver_ix].logic.is_complete() { break cycles }
        }
    }

    pub fn cycle_to_stop_condition(&mut self, stop_condition: &Option<impl Fn(usize, Pulse) -> bool>) -> ExecutionResult {
        (0usize..)
            .map(|_| self.eval_with_stop_condition(stop_condition))
            .take_while_inclusive(|result| !result.terminated_at_stop_condition)
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
    pub cycles: usize,
    pub low_pulse_count: usize,
    pub high_pulse_count: usize,
    pub terminated_at_stop_condition: bool
}

impl ExecutionResult {
    pub fn new(cycles: usize, low_pulse_count: usize, high_pulse_count: usize, terminated_at_stop_condition: bool) -> Self {
        Self { cycles, low_pulse_count, high_pulse_count, terminated_at_stop_condition }
    }
    pub fn product(&self) -> usize {
        self.low_pulse_count * self.high_pulse_count
    }
}

impl Sum for ExecutionResult {
    fn sum<I: Iterator<Item=Self>>(iter: I) -> Self {
        iter.reduce(|acc, x| acc + x)
            .unwrap_or_else(|| ExecutionResult::new(0, 0, 0, false))
    }
}

impl Add for ExecutionResult {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        ExecutionResult::new(
            self.cycles + rhs.cycles,
            self.low_pulse_count + rhs.low_pulse_count,
            self.high_pulse_count + rhs.high_pulse_count,
            self.terminated_at_stop_condition || rhs.terminated_at_stop_condition)
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
    Broadcast,
    FlipFlop,
    Conjunction,
    Terminator, // Undefined modules like 'output' or 'rx' which only act as a receiver
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
    fn is_complete(&self) -> bool;
}

pub fn build_logic(module_type: ModuleType, inputs: &Vec<usize>, _outputs: &Vec<usize>) -> Box<dyn ModuleLogic> {
    match module_type {
        ModuleType::Broadcast => Box::new(Broadcast::new()),
        ModuleType::FlipFlop => Box::new(FlipFlop::new()),
        ModuleType::Conjunction => Box::new(Conjunction::new(&inputs)),
        ModuleType::Terminator => Box::new(Terminator::new())
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
    fn is_complete(&self) -> bool { false }
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
    fn is_complete(&self) -> bool { false }
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
    fn is_complete(&self) -> bool { false }
}
impl Display for Broadcast {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Broadcast")
    }
}

// --- Module implementation: receive-only terminator module, terminates on low signal ---

pub struct Terminator {
    terminate: bool
}

impl Terminator {
    pub fn new() -> Self {
        Self { terminate: false }
    }
}

impl ModuleLogic for Terminator {
    fn get_type(&self) -> ModuleType { ModuleType::Terminator }
    fn evaluate(&mut self, signal: Pulse, _: usize) -> Option<Pulse> {
        if signal == Pulse::Low { self.terminate = true }
        None
    }
    fn is_complete(&self) -> bool { self.terminate }
}
impl Display for Terminator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Terminator(End:{})", if self.terminate { "Y" } else { "N" })
    }
}