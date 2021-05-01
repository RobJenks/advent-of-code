#[derive(Debug, Clone)]
pub struct Criteria {
    fields: Vec<FieldCriteria>
}

#[derive(Debug, Clone)]
pub struct FieldCriteria {
    name: String,
    validity: Vec<InclusiveRange>
}

#[derive(Debug, Clone)]
pub struct InclusiveRange {
    min: u32,
    max: u32
}

impl Criteria {
    pub fn new(fields: Vec<FieldCriteria>) -> Self {
        Self { fields }
    }

    pub fn get_fields(&self) -> &Vec<FieldCriteria> {
        &self.fields
    }

    pub fn accepts_value(&self, value: u32) -> bool {
        self.fields.iter().any(|f| f.accepts_value(value))
    }
}

impl FieldCriteria {
    pub fn new(name: &str, validity: Vec<InclusiveRange>) -> Self {
        Self { name: name.to_string(), validity }
    }

    pub fn get_name(&self) -> &str { self.name.as_str() }

    pub fn accepts_value(&self, value: u32) -> bool {
        self.validity.iter().any(|r| r.contains(value))
    }

    pub fn str(&self) -> String {
        format!("{}: {:?}", self.name,
                self.validity.iter().map(InclusiveRange::str).collect::<Vec<_>>())
    }
}

impl InclusiveRange {
    pub fn new(min: u32, max: u32) -> Self {
        Self { min, max }
    }

    pub fn contains(&self, x: u32) -> bool {
        x >= self.min && x <= self.max
    }

    pub fn str(&self) -> String {
        format!("{}-{}", self.min, self.max)
    }
}