use super::lexical::{Location, Name};

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
        name: Option<Name>,
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
    Meta { name: Option<Name> },
    /// Named reference
    Ref { name: Name },
}

impl AstTerm {
    pub fn location(&self) -> Location {
        unimplemented!()
    }
}

mod tests {
    use super::*;
    use crate::syntax::ast_term::AstTerm::Bind;
    use crate::syntax::ast_term::AstTerm::Meta;
    use crate::syntax::ast_term::Binder::Generalized;

    #[test]
    fn sanity_check() {
        let name = Some(Name {
            text: String::from("name"),
            location: Default::default(),
        });
        let term = Bind {
            name: name.clone(),
            binder: Box::new(Generalized),
            body: Box::new(Meta { name }),
        };
    }
}
