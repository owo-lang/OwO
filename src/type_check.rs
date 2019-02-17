// TypeChecking module

pub mod context;
pub mod core;
pub mod pragma;

use self::context::TCError::*;
use self::context::*;
use self::core::{Def, Term};
use crate::syntax::ast_term::AstTerm;

pub fn is_instance(state: &TCState<Def>, term: &Term, expected_type: &Term) -> Result<(), TCError> {
    Ok(())
}

pub fn to_core(state: &mut TCState<Def>, term: &AstTerm) -> Result<Term, TCError> {
    use crate::syntax::ast_term::Binder::*;
    use crate::syntax::ast_term::ParamVisibility::*;
    match term {
        AstTerm::Meta { name } => Ok(Term::Meta { name: name.clone() }),
        AstTerm::App {
            func,
            arg,
            app_visibility,
        } => {
            let func = to_core(state, func)?;
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
                (Implicit, Explicit) => {
                    let arg = to_core(state, arg)?;
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
                            _ => return Err(IncorrectApplication(
                                arg.location(),
                                String::from("Cannot apply on a non-function"),
                            )),
                        }
                    }
                    Ok(new_func)
                }
                (Implicit, Implicit) | (Explicit, Explicit) => {
                    let arg = to_core(state, arg)?;
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
        AstTerm::Ref { name } => unimplemented!(),
        AstTerm::Bind { name, binder, body } => {
            let mut arg_type = Term::Meta { name: None };
            let mut visibility = Explicit;
            if let Some(name) = name {
                match *binder.clone() {
                    Pi(Some(term), bind_visibility) => {
                        visibility = bind_visibility;
                        arg_type = to_core(state, &term)?;
                    }
                    Pi(None, bind_visibility) => {
                        visibility = bind_visibility;
                    }
                    Lambda(Some(bind_type)) => {
                        arg_type = to_core(state, &bind_type)?;
                    }
                    Lambda(None) => {}
                    Generalized => {}
                }
                state.local_vars.push((name.text.clone(), None));
            }
            let body = to_core(state, body)?;
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

mod tests {
    use crate::syntax::ast_term::Binder::Lambda;
    use crate::syntax::ast_term::ParamVisibility::*;
    use crate::syntax::ast_term::AstTerm::{App, Meta, Bind};
    use crate::syntax::lexical::Name;
    use crate::type_check::to_core;
    use crate::type_check::context::TCState;

    #[test]
    fn app_on_bind() {
        let name = Some(Name {
            text: String::from("name"),
            location: Default::default(),
        });
        let meta = Meta { name: name.clone() };
        let func = Bind {
            name,
            binder: Box::new(Lambda(None)),
            body: Box::new(meta.clone()),
        };
        let arg = meta.clone();
        let app = App {
            arg: Box::new(arg),
            func: Box::new(func),
            app_visibility: Explicit,
        };
        let term = to_core(&mut TCState::default(), &app);
        assert_eq!(term.is_ok(), true);
    }
}
