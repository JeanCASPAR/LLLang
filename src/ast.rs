#![allow(dead_code)]

use std::{collections::HashMap, fmt::Debug};

pub trait Annotation {
    type Ident: Debug;
    type Expr: Debug;
    type Pattern: Debug;
    type Type: Debug;
    type FunDef: Debug;
    type Item: Debug;
}

#[derive(Debug)]
pub enum Type<A: Annotation> {
    Int,
    Ident(A::Ident),
    Param(A::Ident, Vec<(Type<A>, A::Type)>),
    Unit,
    Never,
    OfCourse(Box<(Type<A>, A::Type)>),
    WhyNot(Box<(Type<A>, A::Type)>),
    Tuple(Vec<(Type<A>, A::Type)>),
    Sum(Vec<(Type<A>, A::Type)>),
    Impl(Box<(Type<A>, A::Type)>, Box<(Type<A>, A::Type)>),
    Mu(A::Ident, Box<(Type<A>, A::Type)>),
    Forall(A::Ident, Box<(Type<A>, A::Type)>),
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Lt,
}

#[derive(Debug)]
pub enum Expr<A: Annotation> {
    Integer(isize),
    Ident(A::Ident),
    Param(Box<(Expr<A>, A::Expr)>, Vec<(Type<A>, A::Type)>),
    Unit,
    Inj((Type<A>, A::Type), usize, Box<(Expr<A>, A::Expr)>),
    /// Roll(ty, •) : A[x := µx.A] -> µx.A
    Roll((Type<A>, A::Type), Box<(Expr<A>, A::Expr)>),
    /// Unroll(•) :  µx.A -> A[x := µx.A]
    Unroll(Box<(Expr<A>, A::Expr)>),
    App(Box<(Expr<A>, A::Expr)>, Box<(Expr<A>, A::Expr)>),
    Let(
        (Pattern<A>, A::Pattern),
        Box<(Expr<A>, A::Expr)>,
        Box<(Expr<A>, A::Expr)>,
    ),
    Neg(Box<(Expr<A>, A::Expr)>),
    BinOp(BinOp, Box<(Expr<A>, A::Expr)>, Box<(Expr<A>, A::Expr)>),
    Fun(Box<(FunDef<A>, A::FunDef)>),
    Match(
        Box<(Expr<A>, A::Expr)>,
        Vec<((Pattern<A>, A::Pattern), (Expr<A>, A::Expr))>,
    ),
}

#[derive(Debug)]
pub enum Pattern<A: Annotation> {
    Discard,
    Int(isize),
    Ident(A::Ident),
    Unit,
    Tuple(Vec<(Pattern<A>, A::Pattern)>),
    Inj(usize, Box<(Pattern<A>, A::Pattern)>),
}

#[derive(Debug)]
pub struct FunDef<A: Annotation> {
    pub name: Option<A::Ident>,
    pub ty_var: Vec<A::Ident>,
    pub args: Vec<((Pattern<A>, A::Pattern), (Type<A>, A::Type))>,
    pub ret_ty: (Type<A>, A::Type),
    pub body: (Expr<A>, A::Expr),
    pub rec: bool,
}

#[derive(Debug)]
pub enum Item<A: Annotation> {
    FunDef((FunDef<A>, A::FunDef)),
    TypeDef(A::Ident, (Type<A>, A::Type)),
}

#[derive(Debug)]
pub struct File<'a, A: Annotation> {
    pub items: Vec<(Item<A>, A::Item)>,
    pub idents: Vec<&'a str>,
    pub reverse_idents: HashMap<&'a str, usize>,
}
