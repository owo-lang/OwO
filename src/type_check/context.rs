use std::collections::HashMap;

use crate::syntax::lexical::*;
use crate::type_check::pragma::Pragma;

pub type NameKey = String;
pub type Stack<T> = Vec<T>;

/// Type-checking state
pub struct TCState<T> {
    pub symbol_table: HashMap<NameKey, HashMap<NameKey, T>>,
    /// Name and value. Abstracted value is None, "let"ed value is Some(actual)
    pub local_vars: Stack<(NameKey, Option<T>)>,
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
    /// Cannot find definition of some variable
    UnresolvedReference(Name),
    /// Invalid application
    IncorrectApplication(Location, String),
    /// Don't support application on a meta
    ApplicationOnMeta(Location, String),
    /// Something is not a type
    InvalidType,
}

pub type TCResult<T> = Result<T, TCError>;
