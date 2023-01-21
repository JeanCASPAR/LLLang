use pest::{iterators::*, Parser};
use pest_derive::Parser;

use std::collections::HashMap;

use crate::error::*;

pub type Ident = usize;

#[derive(Parser, Debug)]
#[grammar = "grammar.pest"]
pub struct LLLParser<'a> {
    idents: Vec<&'a str>,
    reverse_idents: HashMap<&'a str, usize>,
}

#[derive(Debug)]
pub enum Type {
    Int,
    Ident(Ident),
    Param(Ident, Vec<Type>),
    Unit,
    Never,
    OfCourse(Box<Type>),
    WhyNot(Box<Type>),
    Tuple(Vec<Type>),
    Sum(Vec<Type>),
    Impl(Box<Type>, Box<Type>),
    Mu(Ident, Box<Type>),
    Forall(Ident, Box<Type>),
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum Expr {
    Integer(isize),
    Ident(Ident),
    Param(Box<Expr>, Vec<Type>),
    Unit,
    Inj(Type, usize, Box<Expr>),
    /// Roll(ty, •) : A[x := µx.A] -> µx.A
    Roll(Type, Box<Expr>),
    /// Unroll(•) :  µx.A -> A[x := µx.A]
    Unroll(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(Pattern, Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Fun(Box<FunDef>),
    Match(Box<Expr>, Vec<(Pattern, Expr)>),
}

#[derive(Debug)]
pub enum Pattern {
    Discard,
    Int(isize),
    Ident(Ident),
    Unit,
    Tuple(Vec<Pattern>),
    Inj(usize, Box<Pattern>),
}

#[derive(Debug)]
pub struct FunDef {
    name: Option<Ident>,
    ty_var: Vec<Ident>,
    args: Vec<(Pattern, Type)>,
    ret_ty: Type,
    body: Expr,
    rec: bool,
}

#[derive(Debug)]
pub enum Item {
    FunDef(FunDef),
    TypeDef(Ident, Type),
}

#[derive(Debug)]
pub struct File<'a> {
    items: Vec<Item>,
    idents: Vec<&'a str>,
    reverse_idents: HashMap<&'a str, usize>,
}

macro_rules! read {
    ($e: expr) => {
        let r = $e.as_rule();
        let mut pairs = $e.into_inner();
        if let Some(pair) = pairs.next() {
            let span = pair.as_span().into();
            let line_column = pair.line_col().into();
            return Err(ErrorType::from(ParseError::TooManyChild(r)).report(span, line_column));
        }
    };
    ($x:ident $(, $y:ident)*; $e:expr) => {
        let r = $e.as_rule();
        let span = $e.as_span().into();
        let line_column = $e.line_col().into();
        let mut pairs = $e.into_inner();
        let $x = if let Some(pair) = pairs.next() {
            pair
            } else {
                return Err(ErrorType::from(ParseError::NotEnoughChild(r)).report(span, line_column))
            };
        $(
            let $y = if let Some(pair) = pairs.next() {
                pair
            } else {
                return Err(ErrorType::from(ParseError::NotEnoughChild(r)).report(span, line_column));
            };
        )*
        if let Some(pair) = pairs.next() {
            let span = pair.as_span().into();
            let line_column = pair.line_col().into();
            return Err(ErrorType::from(ParseError::TooManyChild(r)).report(span, line_column));
        }
    };
}

impl<'a> LLLParser<'a> {
    pub fn new() -> Self {
        let idents = Vec::new();
        let reverse_idents = HashMap::new();

        Self {
            idents,
            reverse_idents,
        }
    }

    pub fn parse(self, data: &'a str) -> Result<File, Error> {
        let mut parsed = <LLLParser as Parser<_>>::parse(Rule::file, data)?;
        let file = if let Some(pair) = parsed.next() {
            pair
        } else {
            return Err(ErrorType::from(ParseError::NotEnoughChild(Rule::file))
                .report(Loc::Pos(0), Loc::Pos((0, 0))));
        };
        if let Some(pair) = parsed.next() {
            let span = pair.as_span().into();
            let line_column = pair.line_col().into();
            return Err(
                ErrorType::from(ParseError::TooManyChild(Rule::file)).report(span, line_column)
            );
        }
        self.parse_file(file)
    }

    fn parse_ident(&mut self, parsed: Pair<'a, Rule>) -> Result<Ident, Error> {
        let span = parsed.as_span().into();
        let line_column = parsed.line_col().into();
        match parsed.as_rule() {
            Rule::ident => {
                let ident = parsed.as_str();
                let entry = self.reverse_idents.entry(ident);
                Ok(*entry.or_insert_with(|| {
                    self.idents.push(ident);
                    self.idents.len() - 1
                }))
            }
            rule => Err(ErrorType::from(ParseError::ExpectedRule(Rule::ident, rule))
                .report(span, line_column)),
        }
    }

    fn parse_int(&mut self, parsed: Pair<Rule>) -> Result<isize, Error> {
        let span = parsed.as_span().into();
        let line_column = parsed.line_col().into();
        match parsed.as_rule() {
            Rule::integer => {
                read!(n; parsed);
                match n.as_rule() {
                    Rule::pos_integer => self.parse_pos_int(n).map(|n| n as _),
                    Rule::neg_integer => {
                        read!(m; n);
                        let n = self.parse_pos_int(m)?;
                        Ok(-(n as isize))
                    }
                    rule => Err(
                        ErrorType::from(ParseError::ExpectedRule(Rule::pos_integer, rule))
                            .report(span, line_column),
                    ),
                }
            }
            rule => Err(
                ErrorType::from(ParseError::ExpectedRule(Rule::integer, rule))
                    .report(span, line_column),
            ),
        }
    }

    fn parse_pos_int(&mut self, parsed: Pair<Rule>) -> Result<usize, Error> {
        let span = parsed.as_span().into();
        let line_column = parsed.line_col().into();
        match parsed.as_rule() {
            Rule::pos_integer => {
                Ok(parsed
                    .as_str()
                    .parse()
                    .map_err(|e: std::num::ParseIntError| {
                        ErrorType::from(ParseError::from(e)).report(span, line_column)
                    })?)
            }
            rule => Err(
                ErrorType::from(ParseError::ExpectedRule(Rule::integer, rule))
                    .report(span, line_column),
            ),
        }
    }

    fn parse_file(mut self, parsed: Pair<'a, Rule>) -> Result<File<'a>, Error> {
        let span = parsed.as_span().into();
        let line_column = parsed.line_col().into();
        let file = if let Rule::file = parsed.as_rule() {
            parsed.into_inner()
        } else {
            return Err(
                ErrorType::from(ParseError::ExpectedRule(Rule::file, parsed.as_rule()))
                    .report(span, line_column),
            );
        };

        let items = file
            .into_iter()
            .filter_map(|item| {
                if matches!(item.as_rule(), Rule::EOI) {
                    None
                } else {
                    Some(self.parse_item(item))
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(File {
            items,
            idents: self.idents,
            reverse_idents: self.reverse_idents,
        })
    }

    fn parse_item(&mut self, parsed: Pair<'a, Rule>) -> Result<Item, Error> {
        let span = parsed.as_span().into();
        let line_column = parsed.line_col().into();
        match parsed.as_rule() {
            Rule::def => {
                read!(def; parsed);
                self.parse_global_fun(def).map(Item::FunDef)
            }
            Rule::rec_def => {
                read!(rec_def; parsed);
                if !matches!(rec_def.as_rule(), Rule::def) {
                    return Err(ErrorType::from(ParseError::ExpectedRule(
                        Rule::rec_def,
                        rec_def.as_rule(),
                    ))
                    .report(span, line_column));
                }
                read!(def; rec_def);
                let mut fun_def = self.parse_global_fun(def)?;
                fun_def.rec = true;
                Ok(Item::FunDef(fun_def))
            }
            Rule::ty_def => {
                read!(ident, ty; parsed);
                let ident = self.parse_ident(ident)?;
                let ty = self.parse_type(ty)?;
                Ok(Item::TypeDef(ident, ty))
            }
            rule => Err(ErrorType::from(ParseError::ExpectedRule(Rule::item, rule))
                .report(span, line_column)),
        }
    }

    fn parse_type(&mut self, parsed: Pair<'a, Rule>) -> Result<Type, Error> {
        let span = parsed.as_span().into();
        let line_column = parsed.line_col().into();
        match parsed.as_rule() {
            Rule::ty_int => {
                read!(parsed);
                Ok(Type::Int)
            }
            Rule::ty_var => {
                read!(var; parsed);
                Ok(Type::Ident(self.parse_ident(var)?))
            }
            Rule::ty_unit => {
                read!(parsed);
                Ok(Type::Unit)
            }
            Rule::ty_never => {
                read!(parsed);
                Ok(Type::Never)
            }
            Rule::ty_impl => {
                read!(lhs, rhs; parsed);
                Ok(Type::Impl(
                    Box::new(self.parse_type(lhs)?),
                    Box::new(self.parse_type(rhs)?),
                ))
            }
            Rule::ty_tuple => Ok(Type::Tuple(
                parsed
                    .into_inner()
                    .map(|pair| self.parse_type(pair))
                    .collect::<Result<_, _>>()?,
            )),
            Rule::ty_disj => Ok(Type::Sum(
                parsed
                    .into_inner()
                    .map(|pair| self.parse_type(pair))
                    .collect::<Result<_, _>>()?,
            )),
            Rule::ty_of_course => {
                read!(ty; parsed);
                Ok(Type::OfCourse(Box::new(self.parse_type(ty)?)))
            }
            Rule::ty_why_not => {
                read!(ty; parsed);
                Ok(Type::WhyNot(Box::new(self.parse_type(ty)?)))
            }
            Rule::ty_param => {
                let mut pairs = parsed.into_inner();
                let pair = if let Some(pair) = pairs.next() {
                    pair
                } else {
                    return Err(ErrorType::from(ParseError::NotEnoughChild(Rule::ty_param))
                        .report(span, line_column));
                };
                let id = self.parse_ident(pair)?;
                let params = pairs
                    .map(|arg| self.parse_type(arg))
                    .collect::<Result<_, _>>()?;
                Ok(Type::Param(id, params))
            }
            Rule::ty_mu => {
                read!(id, ty; parsed);
                read!(id; id);
                let id = self.parse_ident(id)?;
                let ty = self.parse_type(ty)?;
                Ok(Type::Mu(id, Box::new(ty)))
            }
            Rule::ty_forall => {
                read!(id, ty; parsed);
                read!(id; id);
                let id = self.parse_ident(id)?;
                let ty = self.parse_type(ty)?;
                Ok(Type::Forall(id, Box::new(ty)))
            }
            Rule::r#type => {
                read!(ty; parsed);
                self.parse_type(ty)
            }
            rule => Err(
                ErrorType::from(ParseError::ExpectedRule(Rule::r#type, rule))
                    .report(span, line_column),
            ),
        }
    }

    fn parse_pattern(&mut self, parsed: Pair<'a, Rule>) -> Result<Pattern, Error> {
        let span = parsed.as_span().into();
        let line_column = parsed.line_col().into();
        match parsed.as_rule() {
            Rule::pat_discard => Ok(Pattern::Discard),
            Rule::pat_int => {
                read!(integer; parsed);
                Ok(Pattern::Int(self.parse_int(integer)?))
            }
            Rule::pat_var => {
                read!(ident; parsed);
                Ok(Pattern::Ident(self.parse_ident(ident)?))
            }
            Rule::pat_unit => Ok(Pattern::Unit),
            Rule::pat_tuple => {
                let pairs = parsed.into_inner();
                let params = pairs
                    .map(|arg| self.parse_pattern(arg))
                    .collect::<Result<_, _>>()?;
                Ok(Pattern::Tuple(params))
            }
            Rule::pat_inj => {
                read!(integer, pat; parsed);
                Ok(Pattern::Inj(
                    self.parse_int(integer)? as usize,
                    Box::new(self.parse_pattern(pat)?),
                ))
            }
            Rule::pattern => {
                read!(pattern; parsed);
                self.parse_pattern(pattern)
            }
            rule => Err(
                ErrorType::from(ParseError::ExpectedRule(Rule::pattern, rule))
                    .report(span, line_column),
            ),
        }
    }

    fn parse_expr(&mut self, parsed: Pair<'a, Rule>) -> Result<Expr, Error> {
        let span = parsed.as_span().into();
        let line_column = parsed.line_col().into();
        match parsed.as_rule() {
            Rule::e_int => {
                read!(integer; parsed);
                Ok(Expr::Integer(self.parse_int(integer)?))
            }
            Rule::e_var => {
                read!(ident; parsed);
                Ok(Expr::Ident(self.parse_ident(ident)?))
            }
            Rule::e_unit => Ok(Expr::Unit),
            Rule::e_fun => Ok(Expr::Fun(Box::new(self.parse_local_fun(parsed)?))),
            Rule::e_rec_fun => {
                read!(ident, fun; parsed);
                let name = self.parse_ident(ident)?;
                let mut fun = self.parse_local_fun(fun)?;
                fun.name = Some(name);
                fun.rec = true;
                Ok(Expr::Fun(Box::new(fun)))
            }
            Rule::e_param => {
                let mut parsed = parsed.into_inner();
                let e = if let Some(pair) = parsed.next() {
                    self.parse_expr(pair)?
                } else {
                    return Err(ErrorType::from(ParseError::NotEnoughChild(Rule::e_param))
                        .report(span, line_column));
                };
                let ty_var = parsed
                    .map(|pair| self.parse_type(pair))
                    .collect::<Result<_, _>>()?;
                Ok(Expr::Param(Box::new(e), ty_var))
            }
            Rule::e_inj => {
                read!(ty, nb, e; parsed);
                let ty = self.parse_type(ty)?;
                let nb = self.parse_pos_int(nb)?;
                let e = self.parse_expr(e)?;
                Ok(Expr::Inj(ty, nb, Box::new(e)))
            }
            Rule::e_roll => {
                read!(ty, e; parsed);
                let ty = self.parse_type(ty)?;
                let e = self.parse_expr(e)?;
                Ok(Expr::Roll(ty, Box::new(e)))
            }
            Rule::e_unroll => {
                read!(e; parsed);
                let e = self.parse_expr(e)?;
                Ok(Expr::Unroll(Box::new(e)))
            }
            Rule::e_app => {
                read!(e1, e2; parsed);
                let e1 = self.parse_expr(e1)?;
                let e2 = self.parse_expr(e2)?;
                Ok(Expr::App(Box::new(e1), Box::new(e2)))
            }
            Rule::e_let => {
                read!(pat, e1, e2; parsed);
                let pat = self.parse_pattern(pat)?;
                let e1 = self.parse_expr(e1)?;
                let e2 = self.parse_expr(e2)?;
                Ok(Expr::Let(pat, Box::new(e1), Box::new(e2)))
            }
            Rule::e_neg => {
                read!(e; parsed);
                let e = self.parse_expr(e)?;
                Ok(Expr::Neg(Box::new(e)))
            }
            rule @ (Rule::e_add | Rule::e_sub | Rule::e_mul | Rule::e_div | Rule::e_mod) => {
                read!(e1, e2; parsed);
                let e1 = self.parse_expr(e1)?;
                let e2 = self.parse_expr(e2)?;
                let binop = match rule {
                    Rule::e_add => BinOp::Add,
                    Rule::e_sub => BinOp::Sub,
                    Rule::e_mul => BinOp::Mul,
                    Rule::e_div => BinOp::Div,
                    Rule::e_mod => BinOp::Mod,
                    _ => unreachable!(),
                };
                Ok(Expr::BinOp(binop, Box::new(e1), Box::new(e2)))
            }
            Rule::e_match => {
                let mut parsed = parsed.into_inner();
                let matched = if let Some(pair) = parsed.next() {
                    self.parse_expr(pair)?
                } else {
                    return Err(ErrorType::from(ParseError::NotEnoughChild(Rule::e_match))
                        .report(span, line_column));
                };

                let mut last_pattern = None;
                let mut arms = Vec::new();

                for pair in parsed {
                    match pair.as_rule() {
                        Rule::pattern if last_pattern.is_none() => {
                            last_pattern = Some(self.parse_pattern(pair)?);
                        }
                        Rule::expr if last_pattern.is_some() => {
                            let pat = last_pattern.take().unwrap();
                            let e = self.parse_expr(pair)?;
                            arms.push((pat, e));
                        }
                        rule => {
                            return Err(ErrorType::from(ParseError::ExpectedRule(
                                if last_pattern.is_some() {
                                    Rule::expr
                                } else {
                                    Rule::pattern
                                },
                                rule,
                            ))
                            .report(span, line_column))
                        }
                    }
                }

                if last_pattern.is_none() {
                    return Err(ErrorType::from(ParseError::NotEnoughChild(Rule::e_match))
                        .report(span, line_column));
                } else {
                    Ok(Expr::Match(Box::new(matched), arms))
                }
            }
            Rule::expr => {
                read!(expr; parsed);
                self.parse_expr(expr)
            }
            rule => Err(ErrorType::from(ParseError::ExpectedRule(Rule::expr, rule))
                .report(span, line_column)),
        }
    }

    fn parse_fun(&mut self, parsed: Pairs<'a, Rule>) -> Result<FunDef, Error> {
        enum State {
            TypeVar,
            Pattern,
            TypedPattern,
            ReturnType,
        }
        let mut state = State::TypeVar;

        let mut ty_var = Vec::new();
        let mut last_pat = None;
        let mut args = Vec::new();
        let mut ret_ty = None;

        for pair in parsed {
            let span = pair.as_span().into();
            let line_column = pair.line_col().into();
            match (pair.as_rule(), &state) {
                (Rule::ty_var, State::TypeVar) => {
                    read!(id; pair);
                    let name = self.parse_ident(id)?;
                    ty_var.push(name);
                }
                (Rule::pattern, State::TypeVar | State::TypedPattern) => {
                    state = State::Pattern;
                    let pattern = self.parse_pattern(pair)?;
                    last_pat = Some(pattern);
                }
                (Rule::r#type, State::Pattern) => {
                    state = State::TypedPattern;
                    let pat = last_pat.take().unwrap();
                    let ty = self.parse_type(pair)?;
                    args.push((pat, ty));
                }
                (Rule::ret_ty, State::TypedPattern) => {
                    state = State::ReturnType;
                    read!(ty; pair);
                    ret_ty = Some(self.parse_type(ty)?);
                }
                (Rule::expr, State::ReturnType) => {
                    let body = self.parse_expr(pair)?;
                    let fun_def = FunDef {
                        name: None,
                        ty_var,
                        args,
                        ret_ty: ret_ty.unwrap(),
                        body,
                        rec: false,
                    };
                    return Ok(fun_def);
                }
                (rule, state) => {
                    return Err(ErrorType::from(ParseError::ExpectedRule(
                        match state {
                            State::TypeVar => Rule::ty_param,
                            State::Pattern => Rule::r#type,
                            State::TypedPattern => Rule::pattern,
                            State::ReturnType => Rule::expr,
                        },
                        rule,
                    ))
                    .report(span, line_column))
                }
            }
        }

        unreachable!()
    }

    fn parse_local_fun(&mut self, parsed: Pair<'a, Rule>) -> Result<FunDef, Error> {
        let span = parsed.as_span().into();
        let line_column = parsed.line_col().into();
        if !matches!(parsed.as_rule(), Rule::e_fun) {
            return Err(
                ErrorType::from(ParseError::ExpectedRule(Rule::e_fun, parsed.as_rule()))
                    .report(span, line_column),
            );
        }

        self.parse_fun(parsed.into_inner())
    }

    fn parse_global_fun(&mut self, parsed: Pair<'a, Rule>) -> Result<FunDef, Error> {
        let span = parsed.as_span().into();
        let line_column = parsed.line_col().into();
        if !matches!(parsed.as_rule(), Rule::def) {
            return Err(
                ErrorType::from(ParseError::ExpectedRule(Rule::def, parsed.as_rule()))
                    .report(span, line_column),
            );
        }

        let mut parsed = parsed.into_inner();
        let name = if let Some(ident) = parsed.next() {
            self.parse_ident(ident)?
        } else {
            return Err(
                ErrorType::from(ParseError::NotEnoughChild(Rule::def)).report(span, line_column)
            );
        };

        let mut fun_def = self.parse_fun(parsed)?;
        fun_def.name = Some(name);
        Ok(fun_def)
    }
}