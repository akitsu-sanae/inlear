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
        },
        Pair(ref q, box ref t1, box ref t2) => {
            if let &(ref q_, PreType::Pair(box ref ty1, box ref ty2)) = ty {
                if q != q_ {
                    return Err(TypeError::Others(format!("not match: {:?} and {:?}", q, q_)))
                }
                let context = type_check(t1, ty1, context)?;
                let context = type_check(t2, ty2, &context)?;
                if q.check(ty1) && q.check(ty2) {
                    Ok(context)
                } else {
                    Err(TypeError::Others(format!("not satisfy qualitifier condition: {:?}({:?}, {:?})", q, ty1, ty2)))
                }
            } else {
                Err(TypeError::Others(format!("not pair: {:?}", ty)))
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

        print!("type> ");
        std::io::stdout().flush().unwrap();
        input.clear();
        std::io::stdin().read_line(&mut input).unwrap();
        let typ = parse::typ(&input);
        println!("{:?}", typ);
        let typ = match typ {
            Ok(ty) => ty,
            Err(_) => continue,
        };

        if let Err(err) = type_check(&term, &typ, &Context::new()) {
            println!("type error: {:?}", err);
        } else {
            println!("OK!!");
        }
    }
}

