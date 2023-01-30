#![allow(dead_code)]

use scoped_stack::ScopedStack;

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
    type Expr = GlobalLoc;
    type Pattern = GlobalLoc;
    type Type = GlobalLoc;
    type FunDef = GlobalLoc;
    type Item = GlobalLoc;
}

pub struct TypeChecker {
    // type name, arity
    known_types: Vec<(usize, usize)>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            known_types: Vec::new(),
        }
    }

    /// Returns Ok(()) if `name` is the name of a type with the provided arity
    fn assert_type_ident_arity(&self, name: ParseIdent, arity: usize) -> Result<(), Error> {
        for (guess_name, guess_arity) in &self.known_types {
            if name.name == *guess_name {
                return if arity == *guess_arity {
                    Ok(())
                } else {
                    Err(Error {
                        error_type: TypeError::ExpectedArity(name.name, arity, *guess_arity).into(),
                        loc: name.loc,
                    })
                };
            }
        }

        Err(Error {
            error_type: TypeError::UnknownType(name.name).into(),
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
        name_map: &'_ mut ScopedStack<usize, (usize, GlobalLoc)>,
        level: usize,
    ) -> Result<(Type<TypeCheckStage>, <TypeCheckStage as Annotation>::Type), Error> {
        name_map.push_scope();
        let ty = match ty {
            Type::Int => Type::Int,
            Type::Ident(name) => match name_map.get(&name.name) {
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
            Type::Param(name, params) => {
                if name_map.has(&name.name) {
                    return Err(Error {
                        error_type: TypeError::TypeVarWithParam(name.name).into(),
                        loc: name.loc,
                    });
                } else {
                    self.assert_type_ident_arity(name, params.len())
                        .and_then(|_| {
                            if params.is_empty() {
                                Ok(Type::Ident(name))
                            } else {
                                Ok(Type::Param(
                                    name,
                                    params
                                        .into_iter()
                                        .map(|param| self.real_check_type(param, name_map, level))
                                        .collect::<Result<_, _>>()?,
                                ))
                            }
                        })?
                }
            }
            Type::Unit => Type::Unit,
            Type::Never => Type::Never,
            Type::OfCourse(ty) => {
                Type::OfCourse(Box::new(self.real_check_type(*ty, name_map, level)?))
            }
            Type::WhyNot(ty) => Type::WhyNot(Box::new(self.real_check_type(*ty, name_map, level)?)),
            Type::Tuple(ty) => Type::Tuple(
                ty.into_iter()
                    .map(|param| self.real_check_type(param, name_map, level))
                    .collect::<Result<_, _>>()?,
            ),
            Type::Sum(ty) => Type::Sum(
                ty.into_iter()
                    .map(|param| self.real_check_type(param, name_map, level))
                    .collect::<Result<_, _>>()?,
            ),
            Type::Impl(ty1, ty2) => Type::Impl(
                Box::new(self.real_check_type(*ty1, name_map, level)?),
                Box::new(self.real_check_type(*ty2, name_map, level)?),
            ),
            Type::Mu(ty_var, ty) => {
                name_map.insert(ty_var.name, (level, ty_var.loc));
                Type::Mu(
                    ty_var,
                    Box::new(self.real_check_type(*ty, name_map, level + 1)?),
                )
            }
            Type::Forall(ty_var, ty) => {
                name_map.insert(ty_var.name, (level, ty_var.loc));
                Type::Forall(
                    ty_var,
                    Box::new(self.real_check_type(*ty, name_map, level + 1)?),
                )
            }
        };
        name_map.pop_scope();
        Ok((ty, annotation))
    }

    fn check_pattern(
        &self,
        (pattern, annotation): (Pattern<ParserStage>, <ParserStage as Annotation>::Pattern),
        expected_type: &Type<ParserStage>,
        infallible: bool,
    ) -> Result<
        (
            Pattern<TypeCheckStage>,
            <TypeCheckStage as Annotation>::Pattern,
        ),
        Error,
    > {
        let pat = match pattern {
            Pattern::Discard => Pattern::Discard,
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
                        error_type: TypeError::NonInfalliblePattern(pattern, expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                };
                Pattern::Int(n)
            }
            Pattern::Ident(id) => Pattern::Ident(id),
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
                        .map(|(subpat, subty)| self.check_pattern(subpat, &subty.0, infallible))
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
                        error_type: TypeError::NonInfalliblePattern(
                            Pattern::Inj(nb, pat),
                            expected_type.clone(),
                        )
                        .into(),
                        loc: annotation,
                    });
                }
                Pattern::Inj(
                    nb,
                    Box::new(self.check_pattern(*pat, &subtypes[nb].0, infallible)?),
                )
            }
        };
        Ok((pat, annotation))
    }
}
