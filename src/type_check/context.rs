use crate::syntax::lexical::Name;
use crate::type_check::pragma::Pragma;
use std::collections::HashMap;

pub type NameKey = String;

/// Type-checking state
pub struct TCState<T> {
    pub symbol_table: HashMap<NameKey, HashMap<NameKey, T>>,
    pub local_vars: HashMap<NameKey, T>,
    pub pragmas: Vec<Pragma>,
}

/// Type-checking error
pub enum TCError {
    UnresolvedReference(Name),
}
