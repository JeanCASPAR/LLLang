#![allow(dead_code)]

use scoped_stack::ScopedStack;

use std::collections::{hash_map::Entry, HashMap};

use crate::{ast::*, error::*, misc::*, parser::ParserStage};

pub use crate::parser::ParseIdent;

#[derive(Debug, Clone, Copy)]
pub struct DeBruijnIndex {
    /// De Bruijn index
    pub idx: usize,
    /// index in global name table
    pub real_name: usize,
    pub loc: GlobalLoc,
    pub parent_loc: GlobalLoc,
}

impl DeBruijnIndex {
    pub fn new(ident: ParseIdent, idx: usize, parent: GlobalLoc) -> Self {
        let ParseIdent {
            name: real_name,
            loc,
        } = ident;
        Self {
            idx,
            real_name,
            loc,
            parent_loc: parent,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeCheckStage;

impl Annotation for TypeCheckStage {
    type Ident = ParseIdent;
    type TypeVar = DeBruijnIndex;
    type QTypeVar = ParseIdent;
    type TypeParam = Never;
    type Expr = GlobalLoc;
    type Pattern = GlobalLoc;
    type Type = GlobalLoc;
    type FunDef = GlobalLoc;
    type Item = GlobalLoc;
}

pub struct TypeChecker {
    /// type name => type, declaration position, arity
    known_types: HashMap<usize, (Type<TypeCheckStage>, GlobalLoc, usize)>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            known_types: HashMap::new(),
        }
    }

    /// Returns Ok(()) if `name` is the name of a type with the provided arity
    fn assert_type_ident_arity(&self, name: ParseIdent, arity: usize) -> Result<(), Error> {
        let guess_arity = self
            .known_types
            .get(&name.name)
            .ok_or(Error {
                error_type: TypeError::UnknownType(name.name).into(),
                loc: name.loc,
            })?
            .2;
        (guess_arity == arity).then_some(()).ok_or(Error {
            error_type: TypeError::ExpectedArity(name.name, arity, guess_arity).into(),
            loc: name.loc,
        })
    }

    /// Translate a type to the next stage, if it is correct
    fn check_type(
        &self,
        ty: (Type<ParserStage>, <ParserStage as Annotation>::Type),
    ) -> Result<(Type<TypeCheckStage>, <TypeCheckStage as Annotation>::Type), Error> {
        self.real_check_type(ty, &mut ScopedStack::new(), 0)
    }

    /// Same as check type, but take a parameter storing the
    /// type variables already encountered
    fn real_check_type(
        &self,
        (ty, annotation): (Type<ParserStage>, <ParserStage as Annotation>::Type),
        local_types: &'_ mut ScopedStack<usize, (usize, GlobalLoc)>,
        level: usize,
    ) -> Result<(Type<TypeCheckStage>, <TypeCheckStage as Annotation>::Type), Error> {
        local_types.push_scope();
        let ty = match ty {
            Type::Int => Type::Int,
            Type::Ident(name) => match local_types.get(&name.name) {
                Some((original_level, parent_loc)) => Type::TypeVar(DeBruijnIndex::new(
                    name,
                    level - original_level,
                    *parent_loc,
                )),
                None => self
                    .assert_type_ident_arity(name, 0)
                    // We don't get rid of Type::Ident because it is simpler to handle non parametized types
                    .map(|_| Type::Ident(name))?,
            },
            Type::TypeVar(absurd) => match absurd {},
            Type::Param(name, params, ()) => {
                if local_types.has(&name.name) {
                    return Err(Error {
                        error_type: TypeError::TypeVarWithParam(name.name).into(),
                        loc: name.loc,
                    });
                };
                let params = params
                    .into_iter()
                    .map(|ty| {
                        self.real_check_type(ty, local_types, level)
                            .map(|(ty, _)| ty)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                self.assert_type_ident_arity(name, params.len()).map(|()| {
                    let ty = self.known_types.get(&name.name).unwrap().0.clone();
                    ty.specialize(params)
                })?
            }
            Type::Unit => Type::Unit,
            Type::Never => Type::Never,
            Type::OfCourse(ty) => {
                Type::OfCourse(Box::new(self.real_check_type(*ty, local_types, level)?))
            }
            // Type::WhyNot(ty) => Type::WhyNot(Box::new(self.real_check_type(*ty, name_map, level)?)),
            Type::Tuple(ty) => Type::Tuple(
                ty.into_iter()
                    .map(|param| self.real_check_type(param, local_types, level))
                    .collect::<Result<_, _>>()?,
            ),
            Type::Sum(ty) => Type::Sum(
                ty.into_iter()
                    .map(|param| self.real_check_type(param, local_types, level))
                    .collect::<Result<_, _>>()?,
            ),
            Type::Impl(ty1, ty2) => Type::Impl(
                Box::new(self.real_check_type(*ty1, local_types, level)?),
                Box::new(self.real_check_type(*ty2, local_types, level)?),
            ),
            Type::Mu(ty_var, ty) => {
                local_types.insert(ty_var.name, (level, ty_var.loc));
                Type::Mu(
                    ty_var,
                    Box::new(self.real_check_type(*ty, local_types, level + 1)?),
                )
            }
            Type::Forall(ty_var, ty) => {
                local_types.insert(ty_var.name, (level, ty_var.loc));
                Type::Forall(
                    ty_var,
                    Box::new(self.real_check_type(*ty, local_types, level + 1)?),
                )
            }
        };
        local_types.pop_scope();
        Ok((ty, annotation))
    }

    fn check_pattern(
        &self,
        (pattern, annotation): (Pattern<ParserStage>, <ParserStage as Annotation>::Pattern),
        expected_type: &Type<TypeCheckStage>,
        infallible: bool,
        bindings: &mut HashMap<usize, (Type<TypeCheckStage>, GlobalLoc)>,
    ) -> Result<
        (
            Pattern<TypeCheckStage>,
            <TypeCheckStage as Annotation>::Pattern,
        ),
        Error,
    > {
        let pat = match pattern {
            Pattern::Discard => {
                if !matches!(expected_type, Type::OfCourse(_)) {
                    return Err(Error {
                        error_type: TypeError::DiscardLinearExpr(pattern, expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                }
                Pattern::Discard
            }
            Pattern::Int(n) => {
                if !matches!(expected_type, Type::Int) {
                    return Err(Error {
                        error_type: TypeError::NonCompatibleType(pattern, expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                }
                if infallible {
                    return Err(Error {
                        error_type: TypeError::RefutablePattern(pattern, expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                };
                Pattern::Int(n)
            }
            Pattern::Ident(id) => {
                match bindings.entry(id.name) {
                    Entry::Occupied(e) => {
                        let (_, loc) = e.remove();
                        return Err(Error {
                            error_type: TypeError::MultipleBindingPattern(id.name, loc).into(),
                            loc: id.loc,
                        });
                    }
                    Entry::Vacant(e) => {
                        e.insert((expected_type.clone(), id.loc));
                    }
                }

                Pattern::Ident(id)
            }
            Pattern::Unit => {
                if !matches!(expected_type, Type::Unit) {
                    return Err(Error {
                        error_type: TypeError::NonCompatibleType(pattern, expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                }
                Pattern::Unit
            }
            Pattern::Tuple(subpatterns) => {
                let subtypes = if let Type::Tuple(subtypes) = expected_type {
                    subtypes
                } else {
                    return Err(Error {
                        error_type: TypeError::NonCompatibleType(
                            Pattern::Tuple(subpatterns),
                            expected_type.clone(),
                        )
                        .into(),
                        loc: annotation,
                    });
                };
                if subpatterns.len() != subtypes.len() {
                    return Err(Error {
                        error_type: TypeError::NonCompatibleType(
                            Pattern::Tuple(subpatterns),
                            expected_type.clone(),
                        )
                        .into(),
                        loc: annotation,
                    });
                }
                Pattern::Tuple(
                    subpatterns
                        .into_iter()
                        .zip(subtypes)
                        .map(|(subpat, subty)| {
                            self.check_pattern(subpat, &subty.0, infallible, bindings)
                        })
                        .collect::<Result<_, _>>()?,
                )
            }
            Pattern::Inj(nb, pat) => {
                let subtypes = if let Type::Sum(subtypes) = expected_type {
                    subtypes
                } else {
                    return Err(Error {
                        error_type: TypeError::NonCompatibleType(
                            Pattern::Inj(nb, pat),
                            expected_type.clone(),
                        )
                        .into(),
                        loc: annotation,
                    });
                };
                if nb >= subtypes.len() {
                    return Err(Error {
                        error_type: TypeError::NonCompatibleType(
                            Pattern::Inj(nb, pat),
                            expected_type.clone(),
                        )
                        .into(),
                        loc: annotation,
                    });
                }
                if infallible && subtypes.len() != 1 {
                    return Err(Error {
                        error_type: TypeError::RefutablePattern(
                            Pattern::Inj(nb, pat),
                            expected_type.clone(),
                        )
                        .into(),
                        loc: annotation,
                    });
                }
                Pattern::Inj(
                    nb,
                    Box::new(self.check_pattern(*pat, &subtypes[nb].0, infallible, bindings)?),
                )
            }
        };
        Ok((pat, annotation))
    }

    /// bindings : let- and match-introduced variables
    /// local_types : types variables introduced by a polymorphic function
    fn check_expr(
        &self,
        (expr, annotation): (Expr<ParserStage>, <ParserStage as Annotation>::Expr),
        expected_type: &Type<TypeCheckStage>,
        bindings: &mut HashMap<usize, Type<TypeCheckStage>>,
        local_types: &'_ mut ScopedStack<usize, (usize, GlobalLoc)>,
    ) -> Result<(Expr<TypeCheckStage>, <TypeCheckStage as Annotation>::Expr), Error> {
        let e = match expr {
            Expr::Integer(n) => {
                if !matches!(expected_type, Type::Int) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(Type::Int, expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                }
                Expr::Integer(n)
            }
            Expr::Ident(id) => {
                let Some(ty) = bindings.get(&id.name) else {
                    return Err(Error {
                        error_type: TypeError::UnknownVariable(id.name, expected_type.clone())
                            .into(),
                        loc: annotation
                    });
                };
                if ty.eq(expected_type, &self.known_types) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(ty.clone(), expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                }
                if !matches!(expected_type, Type::OfCourse(_)) {
                    bindings.remove(&id.name);
                }

                Expr::Ident(id)
            }
            Expr::Param(_, _) => todo!(),
            Expr::Unit => {
                if !matches!(expected_type, Type::Unit) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(Type::Unit, expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                }

                Expr::Unit
            }
            Expr::Inj(ty, branch, e) => {
                // TODO: not zero!
                let (ty, ty_loc) = self.real_check_type(ty, local_types, 0)?;
                if !ty.eq(expected_type, &self.known_types) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(ty, expected_type.clone()).into(),
                        loc: annotation,
                    });
                }
                let Type::Sum(ty) = ty
                else {
                    return Err(Error {
                        error_type: TypeError::InjNotASum(ty).into(),
                        loc: annotation,
                    });
                };
                if ty.len() <= branch {
                    return Err(Error {
                        error_type: TypeError::SumNotEnoughBranch(Type::Sum(ty), branch).into(),
                        loc: annotation,
                    });
                }

                let e = self.check_expr(*e, &ty[branch].0, bindings, local_types)?;
                Expr::Inj((Type::Sum(ty), ty_loc), branch, Box::new(e))
            }
            Expr::Roll(ty, e) => {
                let (ty, ty_loc) = self.real_check_type(ty, local_types, 0)?;
                if !matches!(ty, Type::Mu(_, _)) {
                    return Err(Error {
                        error_type: TypeError::NonUnrollableType(ty).into(),
                        loc: annotation,
                    });
                }
                let unrolled_ty = ty.clone().unroll();
                if !unrolled_ty.eq(&expected_type, &self.known_types) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(unrolled_ty, expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                }
                let e = self.check_expr(*e, &ty, bindings, local_types)?;
                Expr::Roll((ty, ty_loc), Box::new(e))
            }
            Expr::Unroll(e) => {
                if !matches!(expected_type, Type::Mu(_, _)) {
                    return Err(Error {
                        error_type: TypeError::NonUnrollableType(expected_type.clone()).into(),
                        loc: annotation,
                    });
                }
                let unrolled_ty = expected_type.clone().unroll();
                let e = self.check_expr(*e, &unrolled_ty, bindings, local_types)?;
                Expr::Unroll(Box::new(e))
            }
            // TODO: ici il faut faire de l'inférence de type, pas le choix
            Expr::App(_, _) => todo!(),
            Expr::Let(_, _, _) => todo!(),
            Expr::LetOfCourse(_, _, _) => todo!(),
            Expr::OfCourseLet(_, _, _) => todo!(),
            Expr::Neg(_) => todo!(),
            Expr::BinOp(_, _, _) => todo!(),
            Expr::Fun(_) => todo!(),
            Expr::Match(_, _) => todo!(),
        };
        Ok((e, annotation))
    }
}

impl Type<TypeCheckStage> {
    fn eq(
        &self,
        other: &Self,
        known_types: &HashMap<usize, (Type<TypeCheckStage>, GlobalLoc, usize)>,
    ) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (Type::Ident(id1), Type::Ident(id2)) if id1.name == id2.name => true,
            (Type::Ident(id), ty) | (ty, Type::Ident(id)) => {
                known_types.get(&id.name).unwrap().0.eq(ty, known_types)
            }
            (Type::TypeVar(ty_var1), Type::TypeVar(ty_var2)) => ty_var1.idx == ty_var2.idx,
            (Type::Param(_, _, absurd), _) | (_, Type::Param(_, _, absurd)) => match *absurd {},
            (Type::Unit, Type::Unit) => true,
            (Type::Never, Type::Never) => true,
            (Type::OfCourse(ty1), Type::OfCourse(ty2)) => ty1.0.eq(&ty2.0, known_types),
            (Type::Tuple(ty1), Type::Tuple(ty2)) => {
                ty1.len() == ty2.len()
                    && ty1.iter().zip(ty2).fold(true, |acc, (ty1, ty2)| {
                        acc && Type::eq(&ty1.0, &ty2.0, known_types)
                    })
            }
            (Type::Sum(ty1), Type::Sum(ty2)) => {
                ty1.len() == ty2.len()
                    && ty1
                        .iter()
                        .zip(ty2)
                        .fold(true, |acc, (ty1, ty2)| acc && ty1.0.eq(&ty2.0, known_types))
            }
            (Type::Impl(ty1, ty2), Type::Impl(ty1bis, ty2bis)) => {
                ty1.0.eq(&ty1bis.0, known_types) && ty2.0.eq(&ty2bis.0, known_types)
            }
            (Type::Mu(_, ty1), Type::Mu(_, ty2)) => ty1.0.eq(&ty2.0, known_types),
            (Type::Forall(_, ty1), Type::Forall(_, ty2)) => ty1.0.eq(&ty2.0, known_types),
            _ => false,
        }
    }

    /// Transforme id<A1, A2, ..., An> into the specialized type
    /// id should be an ident, assigned to a type of the form `forall x1, ..., xn . T`
    fn specialize(self, params: Vec<Self>) -> Self {
        let mut ty = self;
        for _ in 0..params.len() {
            let Type::Forall(_, tybis) = ty
            else { unreachable!() };
            ty = tybis.0;
        }
        ty.real_specialize(params, 0)
    }

    fn real_specialize(self, params: Vec<Self>, depth: usize) -> Self {
        match self {
            Type::Int => Type::Int,
            Type::Ident(id) => Type::Ident(id),
            Type::TypeVar(id) => {
                if id.idx < depth || id.idx >= depth + params.len() {
                    Type::TypeVar(id)
                } else {
                    params[id.idx - depth].clone()
                }
            }
            Type::Param(_, _, absurd) => match absurd {},
            Type::Unit => Type::Unit,
            Type::Never => Type::Never,
            Type::OfCourse(ty) => Type::OfCourse(ty),
            Type::Tuple(ty) => Type::Tuple(
                ty.into_iter()
                    .map(|(ty, loc)| (Self::real_specialize(ty, params.clone(), depth), loc))
                    .collect(),
            ),
            Type::Sum(ty) => Type::Sum(
                ty.into_iter()
                    .map(|(ty, loc)| (Self::real_specialize(ty, params.clone(), depth), loc))
                    .collect(),
            ),
            Type::Impl(ty1, ty2) => Type::Impl(
                Box::new((Self::real_specialize(ty1.0, params.clone(), depth), ty1.1)),
                Box::new((Self::real_specialize(ty2.0, params, depth), ty2.1)),
            ),
            Type::Mu(x, ty) => Type::Mu(
                x,
                Box::new((Self::real_specialize(ty.0, params, depth), ty.1)),
            ),
            Type::Forall(x, ty) => Type::Forall(
                x,
                Box::new((Self::real_specialize(ty.0, params, depth + 1), ty.1)),
            ),
        }
    }

    /// unroll(•) :  µx.A -> A[x := µx.A]
    /// not defined on other types
    fn unroll(self) -> Self {
        let this = self.clone();
        let Type::Mu(_, ty) = self
        else { unreachable!() };

        ty.0.specialize(vec![this])
    }
}
