#![allow(dead_code)]

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

/// Linear allocation, could be done better (especially since we can free space early)
#[derive(Debug, Clone, Copy, Eq, Hash)]
pub struct StackIdent {
    /// number of variables above in the stack
    pub n: usize,
    /// number of parent functions
    pub depth: usize,
    /// index in global name table
    pub real_name: usize,
    pub loc: GlobalLoc,
}

impl StackIdent {
    pub fn new(n: usize, depth: usize, real_name: usize, loc: GlobalLoc) -> Self {
        Self {
            n,
            depth,
            real_name,
            loc,
        }
    }
}

impl PartialEq for StackIdent {
    fn eq(&self, other: &Self) -> bool {
        self.loc == other.loc
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeCheckStage;

impl Annotation for TypeCheckStage {
    type Ident = ParseIdent;
    type EIdent = StackIdent;
    type TIdent = Never;
    type TypeVar = DeBruijnIndex;
    type QTypeVar = ();
    type TypeParam = Never;
    type Expr = (
        Type<TypeCheckStage>,
        GlobalLoc,
        // variables used in the expression
        HashMap<StackIdent, Type<TypeCheckStage>>,
        // max stack size (in number of variables)
        usize,
    );
    // max stack size
    type Pattern = (GlobalLoc, usize);
    type Type = GlobalLoc;
    type FunDef = (Type<TypeCheckStage>, GlobalLoc);
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
    /// `local_types` stores the type variables already encountered
    fn check_type(
        &self,
        (ty, annotation): (Type<ParserStage>, <ParserStage as Annotation>::Type),
        local_types: &'_ mut Scope<usize, (usize, GlobalLoc)>,
    ) -> Result<(Type<TypeCheckStage>, <TypeCheckStage as Annotation>::Type), Error> {
        let ty = match ty {
            Type::Int => Type::Int,
            Type::Ident(name) => match local_types.get(&name.name) {
                Some((original_level, parent_loc)) => Type::TypeVar(DeBruijnIndex::new(
                    name,
                    local_types.depth() - original_level,
                    *parent_loc,
                )),
                // we unfold all type definitions
                None => self
                    .known_types
                    .get(&name.name)
                    .ok_or(Error {
                        error_type: TypeError::UnknownType(name.name).into(),
                        loc: name.loc,
                    })?
                    .0
                    .clone(),
            },
            Type::TypeVar(absurd) => match absurd {},
            Type::Param(name, params, ()) => {
                if local_types.contains_key(&name.name) {
                    return Err(Error {
                        error_type: TypeError::TypeVarWithParam(name.name).into(),
                        loc: name.loc,
                    });
                };
                let params = params
                    .into_iter()
                    .map(|ty| self.check_type(ty, local_types).map(|(ty, _)| ty))
                    .collect::<Result<Vec<_>, _>>()?;

                self.assert_type_ident_arity(name, params.len()).map(|()| {
                    let ty = self.known_types.get(&name.name).unwrap().0.clone();
                    ty.specialize(params)
                })?
            }
            Type::Unit => Type::Unit,
            Type::Never => Type::Never,
            Type::OfCourse(ty) => Type::OfCourse(Box::new(self.check_type(*ty, local_types)?)),
            // Type::WhyNot(ty) => Type::WhyNot(Box::new(self.real_check_type(*ty, name_map)?)),
            Type::Tuple(ty) => Type::Tuple(
                ty.into_iter()
                    .map(|param| self.check_type(param, local_types))
                    .collect::<Result<_, _>>()?,
            ),
            Type::Sum(ty) => Type::Sum(
                ty.into_iter()
                    .map(|param| self.check_type(param, local_types))
                    .collect::<Result<_, _>>()?,
            ),
            Type::Impl(ty1, ty2) => Type::Impl(
                Box::new(self.check_type(*ty1, local_types)?),
                Box::new(self.check_type(*ty2, local_types)?),
            ),
            Type::Mu(ty_var, ty) => {
                local_types.insert(
                    ty_var.name,
                    (local_types.depth(), ty_var.loc),
                    !matches!(ty.0, Type::OfCourse(_)),
                );
                local_types.push_scope();
                let ty = Type::Mu((), Box::new(self.check_type(*ty, local_types)?));
                let _ = local_types.pop_scope();
                ty
            }
            Type::Forall(ty_var, ty) => {
                local_types.insert(
                    ty_var.name,
                    (local_types.depth(), ty_var.loc),
                    !matches!(ty.0, Type::OfCourse(_)),
                );
                local_types.push_scope();
                let ty = Type::Forall((), Box::new(self.check_type(*ty, local_types)?));
                let _ = local_types.pop_scope();
                ty
            }
        };
        Ok((ty, annotation))
    }

    /// bindings will contain the newly binded variables
    fn check_pattern(
        &self,
        (pattern, annotation): (Pattern<ParserStage>, <ParserStage as Annotation>::Pattern),
        expected_type: &Type<TypeCheckStage>,
        infallible: bool,
        bindings: &mut HashMap<usize, (Type<TypeCheckStage>, StackIdent)>,
        stack_size: usize,
        depth: usize,
    ) -> Result<
        (
            Pattern<TypeCheckStage>,
            <TypeCheckStage as Annotation>::Pattern,
        ),
        Error,
    > {
        let (pat, max_stack_size) = match pattern {
            Pattern::Discard => {
                if !matches!(expected_type, Type::OfCourse(_)) {
                    return Err(Error {
                        error_type: TypeError::DiscardLinearExpr(pattern, expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                }
                (Pattern::Discard, stack_size)
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
                (Pattern::Int(n), stack_size)
            }
            Pattern::Ident(id) => {
                let stack_id;
                match bindings.entry(id.name) {
                    Entry::Occupied(e) => {
                        let (_, var) = e.remove();
                        return Err(Error {
                            error_type: TypeError::MultipleBindingPattern(id.name, var.loc).into(),
                            loc: id.loc,
                        });
                    }
                    Entry::Vacant(e) => {
                        stack_id = StackIdent::new(stack_size, depth, id.name, id.loc);
                        e.insert((expected_type.clone(), stack_id));
                    }
                }

                (Pattern::Ident(stack_id), stack_size + 1)
            }
            Pattern::Unit => {
                if !matches!(expected_type, Type::Unit) {
                    return Err(Error {
                        error_type: TypeError::NonCompatibleType(pattern, expected_type.clone())
                            .into(),
                        loc: annotation,
                    });
                }
                (Pattern::Unit, stack_size)
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
                let mut new_subpatterns = Vec::new();
                let mut max_stack_size = 0;
                for (subpat, subty) in subpatterns.into_iter().zip(subtypes) {
                    let (subpat, annot) = self.check_pattern(
                        subpat,
                        &subty.0,
                        infallible,
                        bindings,
                        max_stack_size,
                        depth,
                    )?;
                    max_stack_size = annot.1;
                    new_subpatterns.push((subpat, annot));
                }
                (Pattern::Tuple(new_subpatterns), max_stack_size)
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
                let pat = self.check_pattern(
                    *pat,
                    &subtypes[nb].0,
                    infallible,
                    bindings,
                    stack_size,
                    depth,
                )?;
                let max_stack = (pat.1).1;
                (Pattern::Inj(nb, Box::new(pat)), max_stack)
            }
        };
        Ok((pat, (annotation, max_stack_size)))
    }

    /// bindings : let- and match-introduced variables
    /// local_types : types variables introduced by a polymorphic function
    /// stack_size, depth : size of current stack
    /// depth : number of parent functions
    fn check_expr(
        &self,
        (expr, annotation): (Expr<ParserStage>, <ParserStage as Annotation>::Expr),
        bindings: &mut Scope<usize, (Type<TypeCheckStage>, StackIdent)>,
        stack_size: usize,
        depth: usize,
        local_types: &'_ mut Scope<usize, (usize, GlobalLoc)>,
    ) -> Result<(Expr<TypeCheckStage>, <TypeCheckStage as Annotation>::Expr), Error> {
        let (e, ty, used_var, max_stack_size) = match expr {
            Expr::Integer(n) => (Expr::Integer(n), Type::Int, HashMap::new(), stack_size),
            Expr::Ident(id) => {
                let (ty, id) = bindings.get(&id.name).cloned().ok_or(Error {
                    error_type: TypeError::UnknownVariable(id.name).into(),
                    loc: annotation,
                })?;

                if !matches!(ty, Type::OfCourse(_)) {
                    bindings.remove(&id.real_name);
                }
                let mut used_var = HashMap::new();
                used_var.insert(id, ty.clone());

                (Expr::Ident(id), ty, used_var, stack_size)
            }
            Expr::Param(e, params) => {
                let params = params
                    .into_iter()
                    .map(|ty| self.check_type(ty, local_types))
                    .collect::<Result<Vec<_>, _>>()?;
                let e = self.check_expr(*e, bindings, stack_size, depth, local_types)?;
                let (mut ty, mut loc, used_var, max_stack_size) = e.1.clone();
                for _ in 0..params.len() {
                    let Type::Forall(_, tybis) = ty
                    else {
                        return Err(Error { error_type: TypeError::NonParametableType(ty).into(), loc, })
                    };
                    ty = tybis.0;
                    loc = tybis.1;
                }
                let ty = ty.real_specialize(params.iter().map(|(ty, _)| ty.clone()).collect(), 0);
                (
                    Expr::Param(Box::new(e), params),
                    ty,
                    used_var,
                    max_stack_size,
                )
            }
            Expr::Unit => (Expr::Unit, Type::Unit, HashMap::new(), stack_size),
            Expr::Inj(ty, branch, e) => {
                let (ty, ty_loc) = self.check_type(ty, local_types)?;
                let Type::Sum(mut ty) = ty
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

                let e = self.check_expr(*e, bindings, stack_size, depth, local_types)?;
                if !(e.1).0.eq(&ty[branch].0, &self.known_types) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(ty.swap_remove(branch).0, (e.1).0)
                            .into(),
                        loc: annotation,
                    });
                }
                let used_var = (e.1).2.clone();
                let max_stack_size = (e.1).3;
                (
                    Expr::Inj((Type::Sum(ty.clone()), ty_loc), branch, Box::new(e)),
                    Type::Sum(ty),
                    used_var,
                    max_stack_size,
                )
            }
            Expr::Roll(ty, e) => {
                let (ty, ty_loc) = self.check_type(ty, local_types)?;
                if !matches!(ty, Type::Mu(_, _)) {
                    return Err(Error {
                        error_type: TypeError::NonUnrollableType(ty).into(),
                        loc: annotation,
                    });
                }
                let e = self.check_expr(*e, bindings, stack_size, depth, local_types)?;
                let unrolled_ty = ty.clone().unroll();
                if !unrolled_ty.eq(&(e.1).0, &self.known_types) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(unrolled_ty, (e.1).0).into(),
                        loc: annotation,
                    });
                }
                let used_var = (e.1).2.clone();
                let max_stack_size = (e.1).3;
                (
                    Expr::Roll((ty.clone(), ty_loc), Box::new(e)),
                    ty,
                    used_var,
                    max_stack_size,
                )
            }
            Expr::Unroll(e) => {
                let e = self.check_expr(*e, bindings, stack_size, depth, local_types)?;
                if !matches!((e.1).0, Type::Mu(_, _)) {
                    return Err(Error {
                        error_type: TypeError::NonUnrollableType((e.1).0).into(),
                        loc: annotation,
                    });
                }
                let unrolled_ty = (e.1).0.clone().unroll();
                let used_var = (e.1).2.clone();
                let max_stack_size = (e.1).3;
                (
                    Expr::Unroll(Box::new(e)),
                    unrolled_ty,
                    used_var,
                    max_stack_size,
                )
            }
            Expr::App(e1, e2) => {
                let e1 = self.check_expr(*e1, bindings, stack_size, depth, local_types)?;
                let e2 = self.check_expr(*e2, bindings, stack_size, depth, local_types)?;

                let Type::Impl(ty1, ty2) = (e1.1).0.clone()
                else {
                    return Err(Error {
                        error_type: TypeError::ExpectedFunction(e1.0, (e1.1).0).into(),
                        loc: (e1.1).1,
                    });
                };
                let ty1 = ty1.0;
                let ty2 = ty2.0;

                if !(e2.1).0.eq(&ty1, &self.known_types) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(ty1, (e2.1).0).into(),
                        loc: (e2.1).1,
                    });
                }
                let mut used_var = (e1.1).2.clone();
                used_var.extend((e2.1).2.clone());
                let max_stack_size = (e1.1).3.max((e2.1).3);

                (
                    Expr::App(Box::new(e1), Box::new(e2)),
                    ty2,
                    used_var,
                    max_stack_size,
                )
            }
            Expr::Let(pat, e1, e2) => {
                bindings.push_scope();
                let e1 = self.check_expr(*e1, bindings, stack_size, depth, local_types)?;
                let scope = bindings.pop_scope();
                Self::assert_scope_clean(scope)?;
                let mut used_var = (e1.1).2.clone();

                let mut pat_bindings = HashMap::new();
                let pat =
                    self.check_pattern(pat, &(e1.1).0, true, &mut pat_bindings, stack_size, depth)?;

                let mut pat_vars = Vec::new();
                for (_, (ty, var)) in pat_bindings {
                    pat_vars.push(var);
                    Self::linear_insert(var, ty, bindings)?;
                }

                let e2 = self.check_expr(*e2, bindings, (pat.1).1, depth, local_types)?;
                let ty = (e2.1).0.clone();

                used_var.extend((e1.1).2.clone());
                for var in pat_vars.iter() {
                    used_var.remove(var);
                }

                let max_stack_size = (e1.1).3.max((e2.1).3);
                (
                    Expr::Let(pat, Box::new(e1), Box::new(e2)),
                    ty,
                    used_var,
                    max_stack_size,
                )
            }
            Expr::LetOfCourse(x, y, e) => {
                let (ty, y) = bindings.get(&y.name).cloned().ok_or(Error {
                    error_type: TypeError::UnknownVariable(y.name).into(),
                    loc: y.loc,
                })?;
                let Type::OfCourse(ty) = ty
                else {
                    return Err(Error {
                        error_type: TypeError::DuplicateLinearExpression(y.real_name).into(),
                        loc: y.loc,
                    });
                };

                let x = StackIdent::new(stack_size, depth, x.name, x.loc);
                Self::linear_insert(x, ty.0, bindings)?;

                let e = self.check_expr(*e, bindings, stack_size + 1, depth, local_types)?;
                let ty = (e.1).0.clone();

                let mut used_var = (e.1).2.clone();
                used_var.remove(&x);

                let e_ty = (e.1).0.clone();
                let e_loc = (e.1).1;
                let max_stack_size = (e.1).3;

                used_var.insert(y, Type::OfCourse(Box::new((e_ty, e_loc))));

                (
                    Expr::LetOfCourse(x, y, Box::new(e)),
                    ty,
                    used_var,
                    max_stack_size,
                )
            }
            Expr::OfCourseLet(x, e1, e2) => {
                bindings.push_scope();
                bindings.lock();
                let e1 = self.check_expr(*e1, bindings, stack_size, depth, local_types)?;
                bindings.unlock();
                let scope = bindings.pop_scope();

                Self::assert_scope_clean(scope)?;

                let e1_ty = (e1.1).0.clone();
                let e1_ty_loc = (e1.1).1;

                let x = StackIdent::new(stack_size, depth, x.name, x.loc);

                bindings.insert(
                    x.real_name,
                    (Type::OfCourse(Box::new((e1_ty, e1_ty_loc))), x),
                    false,
                );

                let e2 = self.check_expr(*e2, bindings, stack_size + 1, depth, local_types)?;
                let ty = (e2.1).0.clone();

                let mut used_var = (e1.1).2.clone();
                used_var.extend((e2.1).2.clone());
                used_var.remove(&x);

                let max_stack_size = (e1.1).3.max((e2.1).3);

                (
                    Expr::OfCourseLet(x, Box::new(e1), Box::new(e2)),
                    ty,
                    used_var,
                    max_stack_size,
                )
            }
            Expr::Neg(e) => {
                let e = self.check_expr(*e, bindings, stack_size, depth, local_types)?;
                if !(e.1).0.eq(&Type::Int, &self.known_types) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(Type::Int, (e.1).0).into(),
                        loc: annotation,
                    });
                }

                let used_var = (e.1).2.clone();
                let max_stack_size = (e.1).3;
                (Expr::Neg(Box::new(e)), Type::Int, used_var, max_stack_size)
            }
            Expr::BinOp(op, e1, e2) => {
                let e1 = self.check_expr(*e1, bindings, stack_size, depth, local_types)?;
                if !(e1.1).0.eq(&Type::Int, &self.known_types) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(Type::Int, (e1.1).0).into(),
                        loc: annotation,
                    });
                }

                let e2 = self.check_expr(*e2, bindings, stack_size, depth, local_types)?;
                if !(e2.1).0.eq(&Type::Int, &self.known_types) {
                    return Err(Error {
                        error_type: TypeError::MismatchedType(Type::Int, (e2.1).0).into(),
                        loc: annotation,
                    });
                }

                let mut used_var = (e1.1).2.clone();
                used_var.extend((e2.1).2.clone());
                let max_stack_size = (e1.1).3.max((e2.1).3);

                (
                    Expr::BinOp(op, Box::new(e1), Box::new(e2)),
                    Type::Int,
                    used_var,
                    max_stack_size,
                )
            }
            Expr::Fun(fun_def) => {
                let fun_def = self.check_fun_def(*fun_def, bindings, depth, local_types)?;
                let ty = (fun_def.1).0.clone();
                let used_var = todo!("faut que je réfléchisse");
                (Expr::Fun(Box::new(fun_def)), ty, used_var, stack_size + 1)
            }
            Expr::Match(e, branches) => {
                bindings.push_scope();
                let e = self.check_expr(*e, bindings, stack_size, depth, local_types)?;
                let scope = bindings.pop_scope();
                Self::assert_scope_clean(scope)?;

                let mut used_var = (e.1).2.clone();
                let mut common_ty = None;
                let mut new_branches = Vec::new();
                let mut max_stack = (e.1).3;

                let mut used_var_in_first_branch = None;
                let mut used_var_in_last_branch: Option<HashMap<_, _>> = None;
                let mut first_branch_loc = None;
                let mut nonlinear_var = HashMap::new();
                for branch in branches {
                    let (pat, e_branch) = branch;
                    let mut pat_bindings = HashMap::new();

                    let pat = self.check_pattern(
                        pat,
                        &(e.1).0,
                        false,
                        &mut pat_bindings,
                        stack_size,
                        depth,
                    )?;

                    let mut match_bindings = bindings.clone();

                    for (ty, var) in pat_bindings.values() {
                        Self::linear_insert(*var, ty.clone(), &mut match_bindings)?;
                    }

                    match_bindings.push_scope();
                    let e_branch = self.check_expr(
                        e_branch,
                        &mut match_bindings,
                        (pat.1).1,
                        depth,
                        local_types,
                    )?;
                    let scope = match_bindings.pop_scope();
                    Self::assert_scope_clean(scope)?;

                    if let Some(ref common_ty) = common_ty {
                        if !(e_branch.1).0.eq(common_ty, &self.known_types) {
                            return Err(Error {
                                error_type: TypeError::MismatchedType(common_ty.clone(), (e.1).0)
                                    .into(),
                                loc: annotation,
                            });
                        }
                    } else {
                        common_ty = Some((e_branch.1).0.clone());
                    }

                    // check if all branch use the same variables
                    // we check cyclic inclusion, be checking if the variables
                    // of the last visited one are included in the current one
                    if let Some(ref mut used_var_in_last_branch) = used_var_in_last_branch {
                        let mut used_var =
                            HashMap::with_capacity(used_var_in_last_branch.capacity());
                        // inclusion check
                        for (var, ty) in used_var_in_last_branch.iter() {
                            if let Type::OfCourse(_) = ty {
                                nonlinear_var.insert(*var, ty.clone());
                                continue;
                            }
                            if !(e_branch.1).2.contains_key(var) {
                                return Err(Error {
                                    error_type: TypeError::BranchDontConsume(
                                        var.real_name,
                                        var.loc,
                                    )
                                    .into(),
                                    loc: (e_branch.1).1,
                                });
                            }
                        }
                        // update the variables
                        for (var, ty) in &(e_branch.1).2 {
                            if let Type::OfCourse(_) = ty {
                                continue;
                            }
                            used_var.insert(*var, ty.clone());
                        }
                        for (_, (_, var)) in pat_bindings {
                            used_var.remove(&var);
                        }
                        *used_var_in_last_branch = used_var;
                    } else {
                        let mut tmp = (e_branch.1).2.clone();
                        for (_, (_, var)) in pat_bindings {
                            tmp.remove(&var);
                        }
                        used_var_in_first_branch = Some(tmp.clone());
                        used_var_in_last_branch = Some(tmp);
                        first_branch_loc = Some((e_branch.1).1);
                    }

                    max_stack = max_stack.max((e_branch.1).3);
                    new_branches.push((pat, e_branch));
                }

                // cyclic inclusion
                if let (Some(first_branch), Some(last_branch), Some(first_branch_loc)) = (
                    used_var_in_first_branch,
                    used_var_in_last_branch,
                    first_branch_loc,
                ) {
                    for (var, ty) in last_branch {
                        if let Type::OfCourse(_) = ty {
                            nonlinear_var.insert(var, ty.clone());
                            continue;
                        }
                        if !first_branch.contains_key(&var) {
                            return Err(Error {
                                error_type: TypeError::BranchDontConsume(var.real_name, var.loc)
                                    .into(),
                                loc: first_branch_loc,
                            });
                        }
                    }
                    used_var.extend(first_branch);
                    used_var.extend(nonlinear_var);
                }

                // TODO: check d'exhaustivité
                (
                    Expr::Match(Box::new(e), new_branches),
                    common_ty.unwrap(),
                    used_var,
                    0,
                )
            }
        };
        Ok((e, (ty, annotation, used_var, max_stack_size)))
    }

    fn assert_scope_clean(
        scope: HashMap<usize, ((Type<TypeCheckStage>, StackIdent), bool)>,
    ) -> Result<(), Error> {
        for (var, ((ty, id), linear)) in scope {
            if linear {
                return Err(Error {
                    error_type: TypeError::NonUsedLinearVar(var, ty).into(),
                    loc: id.loc,
                });
            }
        }
        Ok(())
    }

    fn linear_insert(
        var: StackIdent,
        ty: Type<TypeCheckStage>,
        bindings: &mut Scope<usize, (Type<TypeCheckStage>, StackIdent)>,
    ) -> Result<(), Error> {
        if let Some(((_, prev_id), linear)) = bindings.remove(&var.real_name) {
            if linear {
                return Err(Error {
                    error_type: TypeError::MultipleBindingPattern(var.real_name, prev_id.loc)
                        .into(),
                    loc: var.loc,
                });
            }
        }

        let linear = !matches!(ty, Type::OfCourse(_));
        bindings.insert(var.real_name, (ty, var), linear);
        Ok(())
    }

    fn check_fun_def(
        &self,
        (fun_def, annotation): (FunDef<ParserStage>, <ParserStage as Annotation>::FunDef),
        bindings: &mut Scope<usize, (Type<TypeCheckStage>, StackIdent)>,
        depth: usize,
        local_types: &'_ mut Scope<usize, (usize, GlobalLoc)>,
    ) -> Result<
        (
            FunDef<TypeCheckStage>,
            <TypeCheckStage as Annotation>::FunDef,
        ),
        Error,
    > {
        let mut vars = fun_def.ty_var.clone();
        vars.sort_unstable_by_key(|var| var.name);
        vars.dedup_by_key(|var| var.name);
        if vars.len() != fun_def.ty_var.len() {
            return Err(Error {
                error_type: TypeError::DuplicateTypeParameter.into(),
                loc: annotation,
            });
        }

        for ty_var in &fun_def.ty_var {
            local_types.insert(ty_var.name, (local_types.depth(), ty_var.loc), false);
            local_types.push_scope();
        }

        let ret_ty = self.check_type(fun_def.ret_ty, local_types)?;

        // let mut arg_bindings = HashMap::new();
        let mut patterns = Vec::with_capacity(fun_def.args.len());
        let mut types = Vec::with_capacity(fun_def.args.len());
        for (pat, ty) in fun_def.args {
            let ty = self.check_type(ty, local_types)?;
            //let pat = self.check_pattern(pat, &ty.0, true, &mut arg_bindings)?;
            let pat = todo!();
            patterns.push(pat);
            types.push(ty);
        }

        let ty = types.iter().cloned().rev().fold(ret_ty.clone(), |acc, ty| {
            let loc = ty.1;
            (Type::Impl(Box::new(ty), Box::new(acc)), loc)
        });

        let args = patterns.into_iter().zip(types).collect();

        let name = fun_def.name.unwrap();
        let fun_def_name = StackIdent::new(0, depth, name.name, name.loc);

        // we push the current function pointer at the top of the stack
        // TODO: nop
        let stack_size = if fun_def.rec {
            Self::linear_insert(fun_def_name, ty.0.clone(), bindings)?;
            1
        } else {
            0
        };

        bindings.push_scope();
        let body = self.check_expr(fun_def.body, bindings, stack_size, depth + 1, local_types)?;
        bindings.pop_scope();

        if !(body.1).0.eq(&ret_ty.0, &self.known_types) {
            return Err(Error {
                error_type: TypeError::MismatchedType(ret_ty.0, (body.1).0).into(),
                loc: annotation,
            });
        }

        for _ in 0..fun_def.ty_var.len() {
            let _ = local_types.pop_scope();
        }

        let fun_def = FunDef {
            name: fun_def.name,
            ty_var: fun_def.ty_var,
            args,
            ret_ty,
            body,
            rec: fun_def.rec,
        };

        Ok((fun_def, (ty.0, annotation)))
    }

    fn is_pattern_matching_exhaustive(_patterns: &[Pattern<TypeCheckStage>]) -> bool {
        todo!()
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
            (Type::Ident(absurd), _) | (_, Type::Ident(absurd)) => match *absurd {},
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

        ty.0.real_specialize(vec![this], 0)
    }

    /// Size, in bytes
    fn size(&self) -> usize {
        match self {
            Type::Int => 8,
            Type::Ident(absurd) => match *absurd {},
            Type::TypeVar(_) => 8, // ptr
            Type::Param(_, _, absurd) => match *absurd {},
            Type::Unit => 0,
            Type::Never => 0,
            Type::OfCourse(ty) => ty.0.size(),
            Type::Tuple(ty) => ty.iter().map(|ty| ty.0.size()).sum(),
            Type::Sum(ty) => {
                let nb = ty.len();
                let max_size = ty.iter().map(|ty| ty.0.size()).max().unwrap();
                let discrim_size = (usize::BITS - 1 - nb.leading_zeros()) as usize; // TODO: discrim_size.ilog2()
                (if discrim_size & 8 == 0 {
                    discrim_size
                } else {
                    discrim_size + 1
                }) + max_size
            }
            Type::Impl(_, _) => 8, // ptr
            Type::Mu(_, ty) => ty.0.size(),
            Type::Forall(_, _) => unimplemented!(),
        }
    }
}
