use super::Command;

pub struct Processor {
    commands: Vec<Command>
}

impl Processor {

}

impl Iterator for Processor {
    type Item = ();

    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}