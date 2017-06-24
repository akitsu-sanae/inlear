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

impl Qual {
    fn check(self, ty: &Type) -> bool {
        match ty {
            &(p, _) => !(self == Qual::Unlinear && p == Qual::Linear),
        }
    }
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

    Others(String),
    Unimplemented
}

fn type_check(t: &Term, context: &Context) -> Result<(Type, Context), TypeError> {
    use Term::*;
    match *t {
        Var(ref name) => {
            match context.get(name) {
                Some(ty) => {
                    match *ty {
                        (Qual::Linear, _) => {
                            let mut context = context.clone();
                            context.remove(name);
                            Ok((ty.clone(), context))
                        },
                        (Qual::Unlinear, _) => Ok((ty.clone(), context.clone()))
                    }
                },
                None => Err(TypeError::UnboundVariable(name.clone())),
            }
        },
        Bool(ref q, _) => Ok(((*q, PreType::Bool), context.clone())),
        If(box ref cond, box ref then, box ref else_) => {
            let (cond_ty, context) = type_check(cond, context)?;
            if let (_, PreType::Bool) = cond_ty {
                let (then_ty, context) = type_check(then, &context)?;
                let (else_ty, context) = type_check(else_, &context)?;
                if then_ty == else_ty {
                    Ok((then_ty, context))
                } else {
                    Err(TypeError::Others(format!("ifのthen節とelse節で返値型が違う")))
                }
            } else {
                Err(TypeError::Others(format!("could not convert {:?} to Boolean", cond_ty)))
            }
        },
        Pair(ref q, box ref t1, box ref t2) => {
            let (ty1, context) = type_check(t1, context)?;
            let (ty2, context) = type_check(t2, &context)?;
            if q.check(&ty1) && q.check(&ty2) {
                let ty = (q.clone(), PreType::Pair(box ty1, box ty2));
                Ok((ty, context))
            } else {
                Err(TypeError::Others(format!("not satisfy qualitifier condition: {:?}({:?}, {:?})", q, ty1, ty2)))
            }
        },
        _ => Err(TypeError::Unimplemented)
    }
}

peg_file! parse("grammar.rustpeg");

fn main() {
    loop {
        use std::io::Write;

        let mut input = String::new();

        print!("term> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        let term = parse::term(&input);
        println!("{:?}", term);
        let term = match term {
            Ok(term) => term,
            Err(_) => continue,
        };

        match type_check(&term, &Context::new()) {
            Ok((typ, _)) => println!("{:?}", typ),
            Err(err) => println!("ERR: {:?}", err),
        }
    }
}

