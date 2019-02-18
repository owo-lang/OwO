use std::collections::HashMap;

use crate::syntax::lexical::*;
use crate::type_check::pragma::Pragma;

pub type NameKey = String;

/// Type-checking state
pub struct TCState<T> {
    pub symbol_table: HashMap<NameKey, HashMap<NameKey, T>>,
    /// Name and value. Abstracted value is None, "let"ed value is Some(actual)
    pub local_vars: Vec<(NameKey, Option<T>)>,
    pub pragmas: Vec<Pragma>,
    /// Non-fatal errors
    pub warnings: Vec<TCError>,
}

/// In this way, [`T`] no longer need to implement [`Default`].
impl<T> Default for TCState<T> {
    fn default() -> Self {
        TCState {
            local_vars: Default::default(),
            pragmas: Default::default(),
            symbol_table: Default::default(),
            warnings: Default::default(),
        }
    }
}

/// Type-checking error
#[derive(Clone)]
pub enum TCError {
    UnresolvedReference(Name),
    IncorrectApplication(Location, String),
    ApplicationOnMeta(Location, String),
}
