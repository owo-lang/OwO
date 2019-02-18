// Abstract syntax, desugared

use super::lexical::{Locatable, Location, Name};

// TODO: Instance argument
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum ParamVisibility {
    Explicit,
    Implicit,
}

#[derive(Clone)]
pub enum Binder {
    /// Lambda, optionally typed
    Lambda(Option<AstTerm>),
    /// Pi type, parameter can be implicit/explicit
    Pi(Option<AstTerm>, ParamVisibility),
    /// Auto-generated type argument
    Generalized,
}

#[derive(Clone)]
pub enum AstTerm {
    Bind {
        name: Name,
        binder: Box<Binder>,
        body: Box<AstTerm>,
    },
    /// Application, applying a term on another term.
    /// Can be implicit/instance application so there's visibility
    App {
        func: Box<AstTerm>,
        arg: Box<AstTerm>,
        app_visibility: ParamVisibility,
    },
    /// Meta variable
    Meta { name: Name },
    /// Named reference
    Ref { name: Name },
}

impl Locatable for AstTerm {
    fn location(&self) -> Location {
        use self::AstTerm::*;
        match self {
            Meta { name } => name.location.clone(),
            Ref { name } => name.location.clone(),
            _ => unimplemented!(),
        }
    }
}
