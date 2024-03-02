use std::{collections::HashMap, hash::Hash};

#[derive(Debug, PartialEq, Eq)]
pub enum Duplicated<L, R> {
    Fwd(R),
    Rev(L),
    Both(R, L),
    Full,
}

pub struct BiMap<L, R> {
    fwd: HashMap<L, R>,
    rev: HashMap<R, L>,
}

impl<L, R> Default for BiMap<L, R> {
    fn default() -> Self {
        Self {
            fwd: HashMap::new(),
            rev: HashMap::new(),
        }
    }
}

impl<L, R> BiMap<L, R>
where
    L: Clone + Eq + Hash,
    R: Clone + Eq + Hash,
{
    pub fn checked_insert(&mut self, l: L, r: R) -> Result<(), Duplicated<L, R>> {
        match (self.fwd.get(&l), self.rev.get(&r)) {
            (None, None) => {
                self.fwd.insert(l.clone(), r.clone());
                self.rev.insert(r, l);
                Ok(())
            }
            (Some(r2), None) => Err(Duplicated::Fwd(r2.clone())),
            (None, Some(l2)) => Err(Duplicated::Rev(l2.clone())),
            (Some(r2), Some(l2)) => {
                if r2 == &r && l2 == &l {
                    Err(Duplicated::Full)
                } else {
                    Err(Duplicated::Both(r2.clone(), l2.clone()))
                }
            }
        }
    }

    pub fn insert(&mut self, l: L, r: R) {
        self.fwd.insert(l.clone(), r.clone());
        self.rev.insert(r, l);
    }

    pub fn get_fwd(&self, l: &L) -> Option<&R> {
        self.fwd.get(l)
    }

    pub fn get_rev(&self, r: &R) -> Option<&L> {
        self.rev.get(r)
    }

    pub fn remove_fwd(&mut self, l: &L) -> Option<R> {
        self.fwd.remove(l).map(|r| {
            self.rev.remove(&r);
            r
        })
    }

    pub fn remove_rev(&mut self, r: &R) -> Option<L> {
        self.rev.remove(r).map(|l| {
            self.fwd.remove(&l);
            l
        })
    }

    pub fn contains_fwd(&self, l: &L) -> bool {
        self.fwd.contains_key(l)
    }

    pub fn contains_rev(&self, r: &R) -> bool {
        self.rev.contains_key(r)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bimap() {
        let mut bimap = BiMap::default();
        assert!(bimap.checked_insert(1, "a".to_string()).is_ok());
        assert!(bimap.checked_insert(2, "b".to_string()).is_ok());
        assert_eq!(
            bimap.checked_insert(1, "c".to_string()),
            Err(Duplicated::Fwd("a".to_string()))
        );
        assert_eq!(
            bimap.checked_insert(3, "b".to_string()),
            Err(Duplicated::Rev(2))
        );
        assert_eq!(
            bimap.checked_insert(1, "a".to_string()),
            Err(Duplicated::Full)
        );
        assert_eq!(
            bimap.checked_insert(2, "b".to_string()),
            Err(Duplicated::Full)
        );
        assert!(bimap.checked_insert(3, "c".to_string()).is_ok());
        assert_eq!(
            bimap.checked_insert(1, "c".to_string()),
            Err(Duplicated::Both("a".to_string(), 3))
        );
        assert_eq!(bimap.get_fwd(&1), Some(&"a".to_string()));
        assert_eq!(bimap.get_rev(&"b".to_string()), Some(&2));
        assert_eq!(bimap.get_fwd(&2), Some(&"b".to_string()));
        assert_eq!(bimap.get_rev(&"a".to_string()), Some(&1));
        assert_eq!(bimap.remove_fwd(&1), Some("a".to_string()));
        assert_eq!(bimap.get_rev(&"a".to_string()), None);
        assert_eq!(bimap.get_fwd(&1), None);
        assert_eq!(bimap.get_rev(&"b".to_string()), Some(&2));
        assert_eq!(bimap.remove_rev(&"b".to_string()), Some(2));
        assert_eq!(bimap.get_fwd(&2), None);
        assert_eq!(bimap.get_rev(&"b".to_string()), None);
        assert_eq!(bimap.get_rev(&"c".to_string()), Some(&3));
    }
}
