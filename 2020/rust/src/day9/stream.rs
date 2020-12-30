use itertools::Itertools;

pub type Val = u64;

pub struct DataStream {
    data: Vec<Val>,
    prelude: usize,
    window: usize
}

impl<'a> DataStream {
    pub fn new(data: Vec<Val>, prelude: usize, window: usize) -> Self {
        assert!(window <= prelude);
        Self { data, prelude, window }
    }

    pub fn iter(&'a self) -> DataStreamIterator<'a> {
        self.into_iter()
    }
}

impl<'a> IntoIterator for &'a DataStream {
    type Item = DataStreamIteratorResult;
    type IntoIter = DataStreamIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        DataStreamIterator {
            stream: self,
            ix: 0
        }
    }
}

pub struct DataStreamIterator<'a> {
    stream: &'a DataStream,
    ix: usize
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DataStreamIteratorResult {
    InPrelude(Val),
    Valid(Val),
    Invalid(Val)
}

impl<'a> Iterator for DataStreamIterator<'a> {
    type Item = DataStreamIteratorResult;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.current_val().map(|val|
            if self.is_in_prelude() {
                DataStreamIteratorResult::InPrelude(*val)
            }
            else {
                self.validate_current_item()
            }
        );

        self.increment();
        result
    }
}

impl<'a> DataStreamIterator<'a> {
    fn current_val(&self) -> Option<&Val> { self.stream.data.get(self.ix) }
    fn current_val_unchecked(&self) -> Val { self.stream.data[self.ix] }

    fn is_in_prelude(&self) -> bool {
        self.ix < self.stream.prelude
    }

    fn increment(&mut self) {
        self.ix += 1;
    }

    fn validate_current_item(&self) -> DataStreamIteratorResult {
        let val = self.current_val_unchecked();
        if let Some(_) = &self.stream.data[(self.ix-self.stream.window)..self.ix].iter()
            .map(|x| *x)
            .tuple_combinations::<(Val, Val)>()
            .filter(|(x0,x1)| (x0 + x1 == val) && (x0 != x1))
            .next() {

            DataStreamIteratorResult::Valid(val)
        }
        else {
            DataStreamIteratorResult::Invalid(val)
        }
    }
}

impl DataStreamIteratorResult {
    pub fn is_valid(&self) -> bool {
        match &self {
            Self::Invalid(_) => false,
            _ => true
        }
    }

    pub fn get_value(&self) -> Val {
        match &self {
            Self::InPrelude(x) => *x,
            Self::Valid(x) => *x,
            Self::Invalid(x) => *x
        }
    }
}