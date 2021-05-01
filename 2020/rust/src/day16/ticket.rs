pub struct Ticket {
    values: Vec<u32>
}

impl Ticket {
    pub fn new(values: Vec<u32>) -> Self {
        Self { values }
    }

    pub fn get_values(&self) -> &Vec<u32> {
        &self.values
    }
}