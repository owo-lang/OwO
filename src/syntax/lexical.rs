#[derive(Clone, Hash)]
pub struct Position {
    pub file_name: String,
    /// Column number, starts from 1
    pub line: u32,
    /// Line number, starts from 1
    pub column: u32,
    /// Absolute position, starts from 0
    pub position: u32,
}

#[derive(Clone, Hash)]
pub struct Location {
    pub start: Position,
    pub end: Position,
}

#[derive(Clone, Hash)]
pub struct Name {
    pub text: String,
    pub location: Location,
}
