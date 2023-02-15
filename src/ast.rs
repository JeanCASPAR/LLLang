use std::{collections::HashMap, fmt::Debug};

pub trait Annotation {
    type Ident: Debug + Clone;
    type EIdent: Debug + Clone;
    type TIdent: Debug + Clone;
    type TypeVar: Debug + Clone;
    /// Q stands for "quantified"
    type QTypeVar: Debug + Clone;
    type TypeParam: Debug + Clone;
    type Expr: Debug + Clone;
    type Pattern: Debug + Clone;
    type Type: Debug + Clone;
    type FunDef: Debug + Clone;
    type Item: Debug + Clone;
}

#[derive(Debug, Clone)]
pub enum Type<A: Annotation> {
    Int,
    Ident(A::TIdent),
    TypeVar(A::TypeVar),
    Param(A::TIdent, Vec<(Type<A>, A::Type)>, A::TypeParam),
    Unit,
    Never,
    OfCourse(Box<(Type<A>, A::Type)>),
    // WhyNot(Box<(Type<A>, A::Type)>),
    Tuple(Vec<(Type<A>, A::Type)>),
    Sum(Vec<(Type<A>, A::Type)>),
    Impl(Box<(Type<A>, A::Type)>, Box<(Type<A>, A::Type)>),
    Mu(A::QTypeVar, Box<(Type<A>, A::Type)>),
    Forall(A::QTypeVar, Box<(Type<A>, A::Type)>),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Lt,
}

#[derive(Debug, Clone)]
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
    /// let x = e in e
    Let(
        (Pattern<A>, A::Pattern),
        Box<(Expr<A>, A::Expr)>,
        Box<(Expr<A>, A::Expr)>,
    ),
    /// let !x = y in e
    LetOfCourse(A::Ident, A::Ident, Box<(Expr<A>, A::Expr)>),
    /// let x = !e in e
    OfCourseLet(A::Ident, Box<(Expr<A>, A::Expr)>, Box<(Expr<A>, A::Expr)>),
    Neg(Box<(Expr<A>, A::Expr)>),
    BinOp(BinOp, Box<(Expr<A>, A::Expr)>, Box<(Expr<A>, A::Expr)>),
    Fun(Box<(FunDef<A>, A::FunDef)>),
    Match(
        Box<(Expr<A>, A::Expr)>,
        Vec<((Pattern<A>, A::Pattern), (Expr<A>, A::Expr))>,
    ),
}

#[derive(Debug, Clone)]
pub enum Pattern<A: Annotation> {
    Discard,
    Int(isize),
    Ident(A::Ident),
    Unit,
    Tuple(Vec<(Pattern<A>, A::Pattern)>),
    Inj(usize, Box<(Pattern<A>, A::Pattern)>),
}

#[derive(Debug, Clone)]
pub struct FunDef<A: Annotation> {
    pub name: Option<A::Ident>,
    pub ty_var: Vec<A::Ident>,
    pub args: Vec<((Pattern<A>, A::Pattern), (Type<A>, A::Type))>,
    pub ret_ty: (Type<A>, A::Type),
    pub body: (Expr<A>, A::Expr),
    pub rec: bool,
}

#[derive(Debug, Clone)]
pub enum Item<A: Annotation> {
    FunDef((FunDef<A>, A::FunDef)),
    TypeDef(A::TIdent, (Type<A>, A::Type)),
}

#[derive(Debug, Clone)]
pub struct File<'a, A: Annotation> {
    pub items: Vec<(Item<A>, A::Item)>,
    pub idents: Vec<&'a str>,
    pub reverse_idents: HashMap<&'a str, usize>,
}
