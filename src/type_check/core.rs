// Core language, meta variables are allowed

use crate::syntax::ast_term::ParamVisibility;
use crate::syntax::lexical::{Location, Name};

/// Core language term
#[derive(Clone)]
pub enum Term {
    App {
        func: Box<Term>,
        arg: Box<Term>,
    },
    Lam {
        arg_type: Box<Term>,
        arg_visibility: ParamVisibility,
        body: Box<Term>,
    },
    /// De-Bruijn Index
    Ref {
        index: u8,
    },
    /// Global reference
    Var {
        name: Name,
    },
    /// Leveled type
    Set {
        level: u8,
    },
    Meta {
        name: Option<Name>,
    },
}

impl Term {
    pub fn location(&self) -> Location {
        unimplemented!()
    }
}

pub struct Def {
    /// Type
    set: Term,
    /// Body
    term: Term,
}
