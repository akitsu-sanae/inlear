#![feature(plugin)]
#![plugin(peg_syntax_ext)]

#![feature(box_patterns)]
#![feature(box_syntax)]

use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Qual {
    Linear,
    Unlinear,
}

impl Qual {
    fn check(self, ty: &Type) -> bool {
        match ty {
            &Type(p, _) => !(self == Qual::Unlinear && p == Qual::Linear),
        }
    }
}

impl fmt::Display for Qual {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Qual::Linear => write!(f, "lin"),
            Qual::Unlinear => write!(f, "un"),
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

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Term::*;
        match *self {
            Var(ref name) => write!(f, "{}", name),
            Bool(ref q, ref b) => write!(f, "{} {}", q, b),
            If(box ref cond, box ref then, box ref else_) =>
                write!(f, "if {} then {} else {}", cond, then, else_),
            Pair(ref q, box ref left, box ref right) =>
                write!(f, "{} ({}, {})", q, left, right),
            Split(ref left, ref right, box ref t1, box ref body) =>
                write!(f, "let {}, {} = {} in {}", left, right, t1, body),
            Lam(ref q, ref arg, ref arg_ty, box ref body) =>
                write!(f, "{} func {}: {} => {}", q, arg, arg_ty, body),
            App(box ref t1, box ref t2) =>
                write!(f, "{} @ {}", t1, t2),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PreType {
    Bool,
    Pair(Box<Type>, Box<Type>),
    Arrow(Box<Type>, Box<Type>),
}

impl fmt::Display for PreType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PreType::*;
        match *self {
            Bool => write!(f, "Bool"),
            Pair(box ref ty1, box ref ty2) => write!(f, "({}, {})", ty1, ty2),
            Arrow(box ref ty1, box ref ty2) => write!(f, "{} -> {}", ty1, ty2),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Type(Qual, PreType);

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.0, self.1)
    }
}

type Context = HashMap<String, Type>;

#[derive(Clone, PartialEq, Eq, Debug)]
enum TypeError {
    UnboundVariable(String),
    IfBranch(Type, Type),
    Expected(Term, Type, Type),
    QualCondition(Type),
    SplitForNonPair(Term),
    ApplyForNonFunction(Term),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TypeError::*;
        match *self {
            UnboundVariable(ref name) => write!(f, "unbound variable: {}", name),
            IfBranch(ref ty1, ref ty2) => write!(f, "then branch and else branch return different type: {} and {}", ty1, ty2),
            Expected(ref t, ref ty1, ref ty2) => write!(f, "the type of  {} is {}, but expected {}", t, ty1, ty2),
            QualCondition(ref ty) => write!(f, "unlinear term can not have linear term: {}", ty),
            SplitForNonPair(ref t) => write!(f, "split non pair term: {}", t),
            ApplyForNonFunction(ref t) => write!(f, "apply for non functional term: {}", t),
        }
    }
}

fn type_check(t: &Term, context: &Context) -> Result<(Type, Context), TypeError> {
    use Term::*;
    match *t {
        Var(ref name) => {
            match context.get(name) {
                Some(ty) => {
                    match *ty {
                        Type(Qual::Linear, _) => {
                            let mut context = context.clone();
                            context.remove(name);
                            Ok((ty.clone(), context))
                        },
                        Type(Qual::Unlinear, _) => Ok((ty.clone(), context.clone()))
                    }
                },
                None => Err(TypeError::UnboundVariable(name.clone())),
            }
        },
        Bool(ref q, _) => Ok((Type(*q, PreType::Bool), context.clone())),
        If(box ref cond, box ref then, box ref else_) => {
            let (cond_ty, context) = type_check(cond, context)?;
            if let Type(_, PreType::Bool) = cond_ty {
                let (then_ty, context) = type_check(then, &context)?;
                let (else_ty, context) = type_check(else_, &context)?;
                if then_ty == else_ty {
                    Ok((then_ty, context))
                } else {
                    Err(TypeError::IfBranch(then_ty, else_ty))
                }
            } else {
                Err(TypeError::Expected(cond.clone(), cond_ty, Type(Qual::Linear, PreType::Bool)))
            }
        },
        Pair(ref q, box ref t1, box ref t2) => {
            let (ty1, context) = type_check(t1, context)?;
            let (ty2, context) = type_check(t2, &context)?;
            let ty1_qcond = q.check(&ty1);
            let ty2_qcond = q.check(&ty2);
            if ty1_qcond && ty2_qcond {
                let ty = Type(q.clone(), PreType::Pair(box ty1, box ty2));
                Ok((ty, context))
            } else if ty1_qcond {
                Err(TypeError::QualCondition(ty1.clone()))
            } else {
                Err(TypeError::QualCondition(ty1.clone()))
            }
        },
        Split(ref left, ref right, box ref t, box ref body) => {
            let (t_ty, context) = type_check(t, context)?;
            if let Type(_, PreType::Pair(box left_ty, box right_ty)) = t_ty {
                let mut context = context.clone();
                context.insert(left.clone(), left_ty);
                context.insert(right.clone(), right_ty);
                let (ty, mut context) = type_check(body, &context)?;
                context.remove(left);
                context.remove(right);
                Ok((ty, context))
            } else {
                Err(TypeError::SplitForNonPair(t.clone()))
            }
        },
        Lam(ref q, ref arg, ref arg_ty, box ref body) => {
            let mut context = context.clone();
            context.insert(arg.clone(), arg_ty.clone());
            let (ret_ty, mut context) = type_check(body, &context)?;
            context.remove(arg);
            let ty = Type(q.clone(), PreType::Arrow(box arg_ty.clone(), box ret_ty));
            Ok((ty, context))
        },
        App(box ref t1, box ref t2) => {
            let (t1_ty, context) = type_check(t1, context)?;
            if let Type(_, PreType::Arrow(box arg_ty, box ret_ty)) = t1_ty {
                let (t2_ty, context) = type_check(t2, &context)?;
                if t2_ty == arg_ty {
                    Ok((ret_ty, context))
                } else {
                    Err(TypeError::Expected(t2.clone(), arg_ty, t2_ty))
                }
            } else {
                Err(TypeError::ApplyForNonFunction(t1.clone()))
            }
        }
    }
}

peg_file! parse("grammar.rustpeg");

fn main() {
    loop {
        use std::io::Write;

        let mut input = String::new();

        print!("> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        let term = parse::term(&input);
        let term = match term {
            Ok(term) => term,
            Err(err) => {
                println!("{:?}", err);
                continue;
            },
        };
        match type_check(&term, &Context::new()) {
            Ok((typ, _)) => println!("{}", typ),
            Err(err) => println!("ERR: {}", err),
        }
    }
}

#[test]
fn if_test() {
    let term = parse::term("if lin true then un true else un false").unwrap();
    assert_eq!(term, Term::If(
            box Term::Bool(Qual::Linear, true),
            box Term::Bool(Qual::Unlinear, true),
            box Term::Bool(Qual::Unlinear, false)));
    let (ty, _) = type_check(&term, &Context::new()).unwrap();
    assert_eq!(ty, (Qual::Unlinear, PreType::Bool));
}

#[test]
fn split_test() {
    let term = parse::term("let left, right = lin (lin true, un false) in left").unwrap();
    assert_eq!(term, Term::Split(
            "left".to_string(), "right".to_string(),
            box Term::Pair(
                Qual::Linear,
                box Term::Bool(Qual::Linear, true),
                box Term::Bool(Qual::Unlinear, false)),
            box Term::Var("left".to_string())));
    let (ty, _) = type_check(&term, &Context::new()).unwrap();
    assert_eq!(ty, (Qual::Linear, PreType::Bool));
}

