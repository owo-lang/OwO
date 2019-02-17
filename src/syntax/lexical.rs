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

#[derive(Clone, Hash)]
pub struct Name {
    pub text: String,
    pub location: Location,
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
