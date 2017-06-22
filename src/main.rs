#![feature(plugin)]
#![plugin(peg_syntax_ext)]

#![feature(box_patterns)]
#![feature(box_syntax)]

use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Qual {
    Linear,
    Unlinear,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Term {
    Var(String),
    Bool(Qual, bool),
    If(Box<Term>, Box<Term>, Box<Term>),
    Pair(Qual, Box<Term>, Box<Term>),
    Split(String, String, Box<Term>, Box<Term>),
    Lam(Qual, String, Type, Box<Term>),
    App(Box<Term>, Box<Term>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PreType {
    Bool,
    Pair(Box<Type>, Box<Type>),
    Arrow(Box<Type>, Box<Type>),
}

pub type Type = (Qual, PreType);

type Context = HashMap<String, Type>;

#[derive(Clone, PartialEq, Eq, Debug)]
enum TypeError {
    UnboundVariable(String),
    VariableType(String, Type, Type),
    Unmatch(Type, Type),

    Unimplemented
}

fn type_check(t: &Term, ty: &Type, context: &Context) -> Result<Context, TypeError> {
    use Term::*;
    match *t {
        Var(ref name) => {
            match context.get(name) {
                Some(ty_) if ty == ty_ => {
                    match *ty {
                        (Qual::Linear, _) => {
                            let mut context = context.clone();
                            context.remove(name);
                            Ok(context)
                        },
                        (Qual::Unlinear, _) => Ok(context.clone())
                    }
                },
                Some(ty_) => Err(TypeError::VariableType(name.clone(), ty.clone(), ty_.clone())),
                None => Err(TypeError::UnboundVariable(name.clone())),
            }
        },
        Bool(ref q, _) => {
            if *ty == (*q, PreType::Bool) {
                Ok(context.clone())
            } else {
                Err(TypeError::Unmatch(ty.clone(), (*q, PreType::Bool)))
            }
        },
        If(box ref cond, box ref then, box ref else_) => {
            let context = type_check(cond, &(Qual::Unlinear, PreType::Bool), context)?;
            let context = type_check(then, ty, &context)?;
            let context = type_check(else_, ty, &context)?;
            Ok(context)
        }
        _ => Err(TypeError::Unimplemented)
    }
}

peg_file! parse("grammar.rustpeg");

fn main() {
    loop {
        use std::io::Write;

        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        println!("{:?}", parse::term(&input));
    }
}

