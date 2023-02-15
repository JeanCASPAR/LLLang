pub use self::scope::Scope;

/// Shall be replaced by `pub type Never = !;` when
/// [feature(never_type)](https://github.com/rust-lang/rust/issues/35121) is stabilized
#[derive(Debug, Clone, Copy)]
pub enum Never {}

#[derive(Debug, Clone, Copy)]
pub enum Loc<T> {
    Pos(T),
    Span(T, T),
}

impl From<pest::Span<'_>> for Loc<usize> {
    fn from(value: pest::Span) -> Self {
        Loc::Span(value.start(), value.end())
    }
}

impl From<(usize, usize)> for Loc<(usize, usize)> {
    fn from(value: (usize, usize)) -> Self {
        Loc::Pos(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalLoc {
    pub pos: Loc<usize>,
    pub line_column: Loc<(usize, usize)>,
}

impl GlobalLoc {
    pub fn new(pos: Loc<usize>, line_column: Loc<(usize, usize)>) -> Self {
        Self { pos, line_column }
    }
}

mod scope {
    use std::{borrow::Borrow, collections::HashMap, hash::Hash};

    #[derive(Clone)]
    pub struct Scope<K, V> {
        current: HashMap<K, (V, bool)>,
        parents: Vec<HashMap<K, (V, bool)>>,
        locks: Vec<usize>,
    }

    impl<K, V> Scope<K, V> {
        pub fn new() -> Self {
            Self {
                current: HashMap::new(),
                parents: Vec::new(),
                locks: Vec::new(),
            }
        }

        pub fn depth(&self) -> usize {
            self.parents.len()
        }

        fn iter_scope(&self) -> impl Iterator<Item = &HashMap<K, (V, bool)>> {
            std::iter::once(&self.current).chain(&self.parents)
        }

        fn iter_scope_mut(&mut self) -> impl Iterator<Item = &mut HashMap<K, (V, bool)>> {
            std::iter::once(&mut self.current).chain(&mut self.parents)
        }

        pub fn push_scope(&mut self) {
            let current = std::mem::take(&mut self.current);
            self.parents.push(current);
        }

        pub fn pop_scope(&mut self) -> HashMap<K, (V, bool)> {
            let cur = std::mem::take(&mut self.current);
            self.current = self.parents.pop().expect("Can't pop from empty stack");
            cur
        }

        /// When a scope is locked, only non-lockable variables can be accessed
        /// in the scope and in his parents
        pub fn lock(&mut self) {
            if self
                .locks
                .last()
                .map(|lock| *lock != self.parents.len())
                .unwrap_or(false)
            {
                self.locks.push(self.parents.len());
            }
        }

        pub fn unlock(&mut self) {
            if self.locks.is_empty() {
                panic!("Can't remove a lock when there is none")
            }
            if *self.locks.last().unwrap() == self.parents.len() {
                self.locks.pop();
            } else {
                panic!("Can't unlock a non-locked scope")
            }
        }
    }

    impl<K, V> Scope<K, V>
    where
        K: Eq + Hash,
    {
        pub fn contains_key<Q>(&self, k: &Q) -> bool
        where
            K: Borrow<Q>,
            Q: Hash + Eq + ?Sized,
        {
            self.iter_scope().any(|table| table.contains_key(k))
        }

        pub fn get<Q>(&self, k: &Q) -> Option<&V>
        where
            K: Borrow<Q>,
            Q: Hash + Eq + ?Sized,
        {
            let n = self.parents.len();
            self.iter_scope()
                .enumerate()
                .filter_map(|(idx, table)| match table.get(k) {
                    Some((v, lockable)) => {
                        if *lockable && self.locks.iter().rev().find(|x| **x <= n - idx).is_some() {
                            None
                        } else {
                            Some(v)
                        }
                    }
                    None => None,
                })
                .next()
        }

        pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
        where
            K: Borrow<Q>,
            Q: Hash + Eq + ?Sized,
        {
            let n = self.parents.len();
            // Rust can't see that self.iter_scope_mut only borrows mutable
            // self.scopes and don't touch self.scopes
            let locks = self.locks.clone();
            self.iter_scope_mut()
                .enumerate()
                .filter_map(|(idx, table)| match table.get_mut(k) {
                    Some((v, lockable)) => {
                        if *lockable && locks.iter().rev().find(|x| **x <= n - idx).is_some() {
                            None
                        } else {
                            Some(v)
                        }
                    }
                    None => None,
                })
                .next()
        }

        pub fn insert(&mut self, k: K, v: V, lockable: bool) {
            let present = self.current.insert(k, (v, lockable));
            if present.is_some() {
                panic!("Inside a scope, the bindings are permanent");
            }
        }

        pub fn remove<Q>(&mut self, k: &Q) -> Option<(V, bool)>
        where
            K: Borrow<Q>,
            Q: Hash + Eq + ?Sized,
        {
            self.iter_scope_mut().find_map(|table| table.remove(k))
        }
    }
}
