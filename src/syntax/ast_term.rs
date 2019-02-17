use super::lexical::Name;

// TODO: Instance argument
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum ParamVisibility {
    Explicit,
    Implicit,
}

pub enum Binder {
    /// Lambda, optionally typed
    Lambda(Option<AstTerm>),
    /// Pi type, parameter can be implicit/explicit
    Pi(Option<AstTerm>, ParamVisibility),
    /// Auto-generated type argument
    Generalized,
}

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
        app_type: ParamVisibility,
    },
    /// Meta variable
    Meta { name: Option<Name> },
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
        });
        let term = Bind {
            name: name.clone(),
            binder: Box::new(Generalized),
            body: Box::new(Meta { name }),
        };
    }
}
