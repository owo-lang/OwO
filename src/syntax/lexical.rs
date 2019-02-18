// Position, Location, Names

use std::fmt::{Debug, Error, Formatter};

#[derive(Clone, Hash)]
pub struct Position {
    /// Column number, starts from 1
    pub line: u32,
    /// Line number, starts from 1
    pub column: u32,
    /// Absolute position, starts from 0
    pub position: u32,
}

#[derive(Clone, Hash, Default)]
pub struct Location {
    pub file_name: String,
    pub start: Position,
    pub end: Position,
}

pub trait Locatable {
    fn location(&self) -> Location;
}

#[derive(Clone, Hash)]
pub struct Name {
    pub text_name: Option<String>,
    pub location: Location,
}

impl Name {
    pub fn pretty_text(&self) -> String {
        self.text_name.clone().unwrap_or(String::from("anonymous"))
    }
}

impl Debug for Name {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        f.write_str(self.pretty_text().as_str())
    }
}

impl Default for Position {
    fn default() -> Self {
        Position {
            line: 1,
            column: 1,
            position: 0,
        }
    }
}
