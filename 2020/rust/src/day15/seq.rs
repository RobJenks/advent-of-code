use std::collections::HashMap;

#[derive(Debug)]
pub struct Seq {
    turn: usize,
    last: usize,
    init: Vec<usize>,
    history: HashMap<usize, usize>
}

impl Seq {
    pub fn new(init: Vec<usize>) -> Self {
        Self {
            turn: 0,
            last: 0,
            init: init.into_iter().rev().collect(),  // Store init in reverse order
            history: HashMap::new()
        }
    }

    fn record_new_value(&mut self, val: usize) {
        self.last = val;
    }

    fn commit_history(&mut self) {
        self.history.insert(self.last,self.turn - 1);
    }

    fn determine_new_value(&self) -> usize {
        if let Some(turn) = self.history.get(&self.last) {
            (self.turn - 1) - turn
        }
        else { 0 }
    }
}

impl Iterator for Seq {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        self.turn += 1;
        if let Some(init_value) = self.init.pop() {
            if self.turn != 1 { self.commit_history(); }
            self.record_new_value(init_value);
            Some(init_value)
        }
        else {
            let new_value = self.determine_new_value();
            self.commit_history();
            self.record_new_value(new_value);
            Some(new_value)
        }
    }
}