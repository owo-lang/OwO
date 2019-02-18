use crate::syntax::abs::AstTerm;
use crate::syntax::abs::ParamVisibility::*;
use crate::syntax::elab::{Def, Term};
use crate::type_check::context::TCError;
use crate::type_check::context::TCError::*;
use crate::type_check::context::TCState;

pub fn is_instance(state: &TCState<Def>, term: &Term, expected_type: &Term) -> Result<(), TCError> {
    Ok(())
}

pub fn elaborate(state: &mut TCState<Def>, term: &AstTerm) -> Result<Term, TCError> {
    use crate::syntax::abs::Binder::*;
    match term {
        AstTerm::Meta { name } => Ok(Term::Meta { name: name.clone() }),
        AstTerm::App {
            func,
            arg,
            app_visibility,
        } => {
            let func = elaborate(state, func)?;
            let (arg_visibility, arg_type, body) = match &func {
                Term::Lam {
                    arg_type,
                    arg_visibility,
                    body,
                } => (arg_visibility, arg_type, body),
                _ => {
                    return Err(IncorrectApplication(
                        term.location(),
                        String::from("Cannot apply on a non-function"),
                    ));
                }
            };
            match (app_visibility, arg_visibility) {
                (Implicit, Explicit) => explicitly_apply_on_implicit(state, arg, &func, body),
                (Implicit, Implicit) | (Explicit, Explicit) => {
                    let arg = elaborate(state, arg)?;
                    is_instance(state, &arg, arg_type).map(|()| Term::App {
                        arg: Box::new(arg),
                        func: Box::new(func),
                    })
                }
                (Explicit, Implicit) => Err(IncorrectApplication(
                    term.location(),
                    String::from("Cannot implicitly apply on an explicit parameter"),
                )),
            }
        }
        AstTerm::Ref { name } => {
            if let Some(index) = state
                .local_vars
                .iter()
                .position(|(def_name, _)| def_name == &name.text)
            {
                Ok(Term::Var { index })
            } else {
                // TODO: global reference
                Err(UnresolvedReference(name.clone()))
            }
        }
        AstTerm::Bind { name, binder, body } => {
            let mut arg_type = Term::Meta { name: None };
            let mut visibility = Explicit;
            if let Some(name) = name {
                match *binder.clone() {
                    Pi(Some(term), bind_visibility) => {
                        visibility = bind_visibility;
                        arg_type = elaborate(state, &term)?;
                    }
                    Pi(None, bind_visibility) => {
                        visibility = bind_visibility;
                    }
                    Lambda(Some(bind_type)) => {
                        arg_type = elaborate(state, &bind_type)?;
                    }
                    Lambda(None) => {}
                    Generalized => {}
                }
                state.local_vars.push((name.text.clone(), None));
            }
            let body = elaborate(state, body)?;
            if name.is_some() {
                state.local_vars.pop().unwrap();
            }
            Ok(Term::Lam {
                body: Box::new(body),
                arg_visibility: Explicit,
                arg_type: Box::new(arg_type),
            })
        }
    }
}

/// A very long part of [`to_core`], extracted
fn explicitly_apply_on_implicit(
    state: &mut TCState<Def>,
    arg: &Box<AstTerm>,
    func: &Term,
    body: &Box<Term>,
) -> Result<Term, TCError> {
    let arg = elaborate(state, arg)?;
    let mut new_func = Term::App {
        func: Box::new(func.clone()),
        arg: Box::new(Term::Meta { name: None }),
    };
    loop {
        match *body.clone() {
            Term::Lam {
                arg_visibility: Explicit,
                arg_type,
                body,
            } => {
                new_func = Term::App {
                    func: Box::new(new_func),
                    arg: Box::new(arg),
                };
                break;
            }
            Term::Lam {
                arg_visibility: Implicit,
                arg_type,
                body,
            } => {
                new_func = Term::App {
                    func: Box::new(new_func),
                    arg: Box::new(Term::Meta { name: None }),
                };
            }
            _ => {
                return Err(IncorrectApplication(
                    arg.location(),
                    String::from("Cannot apply on a non-function"),
                ));
            }
        }
    }
    Ok(new_func)
}

mod tests {
    use super::elaborate;
    use crate::syntax::abs::AstTerm::{App, Bind, Meta, Ref};
    use crate::syntax::abs::Binder::Lambda;
    use crate::syntax::abs::ParamVisibility::*;
    use crate::syntax::lexical::Name;
    use crate::type_check::context::TCError::*;
    use crate::type_check::context::TCState;

    #[test]
    #[rustfmt::skip]
    fn app_on_bind() {
        let name = Some(Name { text: String::from("name"), location: Default::default() });
        let reference = Ref { name: name.clone().unwrap() };
        let arg = Meta { name: name.clone() };
        let func = Bind { name, binder: Box::new(Lambda(None)), body: Box::new(reference) };
        let app = App { arg: Box::new(arg), func: Box::new(func), app_visibility: Explicit };
        let term = elaborate(&mut TCState::default(), &app);
        assert_eq!(term.is_ok(), true);
    }

    #[test]
    #[rustfmt::skip]
    fn unresolved_reference() {
        let name = Some(Name { text: String::from("name"), location: Default::default() });
        let wrong_name = Name { text: String::from("a"), location: Default::default() };
        let reference = Ref { name: wrong_name };
        let arg = Meta { name: name.clone() };
        let func = Bind { name, binder: Box::new(Lambda(None)), body: Box::new(reference) };
        let app = App { arg: Box::new(arg), func: Box::new(func), app_visibility: Explicit };
        let term = elaborate(&mut TCState::default(), &app);
        match term {
            Err(UnresolvedReference(_)) => {}
            _ => panic!("test failed")
        };
    }
}
