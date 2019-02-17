use crate::syntax::lexical::*;
use crate::type_check::pragma::Pragma;
use std::collections::HashMap;

pub type NameKey = String;

/// Type-checking state
pub struct TCState<T> {
    pub symbol_table: HashMap<NameKey, HashMap<NameKey, T>>,
    /// Name and value. Abstracted value is None, "let"ed value is Some(actual)
    pub local_vars: Vec<(NameKey, Option<T>)>,
    pub pragmas: Vec<Pragma>,
}

/// Type-checking error
#[derive(Clone)]
pub enum TCError {
    UnresolvedReference(Name),
    IncorrectApplication(Location, String),
}
