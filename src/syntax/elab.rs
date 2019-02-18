// Elaborated syntax, meta variables are allowed

use crate::syntax::abs::ParamVisibility;
use crate::syntax::lexical::{Locatable, Location, Name};

/// Core language term
#[derive(Clone, Debug)]
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
    Var {
        index: usize,
    },
    /// Global reference
    Ref {
        name: Name,
    },
    /// Leveled type
    Set {
        level: u8,
    },
    Meta {
        name: Name,
    },
}

impl Locatable for Term {
    fn location(&self) -> Location {
        use self::Term::*;
        match self {
            Meta { name } => name.location.clone(),
            Ref { name } => name.location.clone(),
            _ => unimplemented!(),
        }
    }
}

impl Term {
    pub fn anonymous_meta(location: Location) -> Term {
        Term::Meta {
            name: Name {
                text_name: None,
                location,
            },
        }
    }
}

pub struct Def {
    /// Type
    set: Term,
    /// Body
    term: Term,
}
