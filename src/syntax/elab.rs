// Elaborated syntax, meta variables are allowed

use crate::syntax::abs::ParamVisibility;
use crate::syntax::lexical::{Locatable, Location, Name};

/// Constrains on a meta var. Currently I decide to only make
/// a simple trivial meta solver.
#[derive(Clone, Debug)]
pub enum MetaConstraint {
    IsTypeOf(Term),
    IsInstanceOf(Term),
    IsEquivalentTo(Term),
}

/// Core language term
#[derive(Clone, Debug)]
pub enum Term {
    App {
        func: Box<Term>,
        arg: Box<Term>,
    },
    Bind {
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
    // TODO level
    Type,
    Meta {
        name: Name,
        constraints: Vec<Box<MetaConstraint>>,
    },
}

impl Locatable for Term {
    fn location(&self) -> Location {
        use self::Term::*;
        match self {
            Meta { name, constraints } => name.location.clone(),
            Ref { name } => name.location.clone(),
            _ => unimplemented!(),
        }
    }
}

impl Term {
    pub fn fresh_meta(name: Name) -> Term {
        Term::Meta {
            name,
            constraints: vec![],
        }
    }

    pub fn anonymous_meta(location: Location) -> Term {
        Term::fresh_meta(Name {
            text_name: None,
            location,
        })
    }
}

pub struct Def {
    /// Type
    set: Term,
    /// Body
    term: Term,
}
