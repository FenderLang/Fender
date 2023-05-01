use std::{
    cell::{Cell, RefCell},
    fmt::Display,
    hash::Hash,
    ops::Index,
};

#[derive(Debug, Clone)]
pub struct FenderString {
    chars: Vec<char>,
    has_changed: Cell<bool>,
    cached_string: RefCell<String>,
}

impl FenderString {
    pub fn new(chars: Vec<char>) -> FenderString {
        FenderString {
            chars,
            has_changed: Cell::new(true),
            cached_string: Default::default(),
        }
    }
}

impl FenderString {
    pub fn escaped_string(&self) -> String {
        format!("{:?}", self.to_string())
    }

    pub fn is_empty(&self) -> bool {
        self.chars.is_empty()
    }

    pub fn len(&self) -> usize {
        self.chars.len()
    }

    pub fn push_str(&mut self, other: &str) {
        if other.is_empty() {
            return;
        }

        self.has_changed.set(true);
        self.chars.extend(other.chars())
    }

    pub fn push_fndr_str(&mut self, other: &FenderString) {
        if other.is_empty() {
            return;
        }
        self.has_changed.set(true);
        self.chars.extend(other.chars.clone())
    }

    pub fn push_char(&mut self, other: char) {
        self.has_changed.set(true);
        self.chars.push(other)
    }

    pub fn remove(&mut self, pos: usize) -> Option<char> {
        if pos >= self.len() {
            None
        } else {
            self.has_changed.set(true);
            Some(self.chars.remove(pos))
        }
    }

    pub fn to_uppercase(&self) -> FenderString {
        FenderString::new(
            self.chars
                .iter()
                .map(|c| c.to_uppercase().next().unwrap())
                .collect(),
        )
    }
    pub fn to_lowercase(&self) -> FenderString {
        FenderString::new(
            self.chars
                .iter()
                .map(|c| c.to_lowercase().next().unwrap())
                .collect(),
        )
    }

    pub fn iter(&self) -> std::slice::Iter<'_, char> {
        self.chars.iter()
    }

    pub fn chars(&self) -> &Vec<char> {
        &self.chars
    }
}

impl Index<usize> for FenderString {
    type Output = char;

    fn index(&self, index: usize) -> &Self::Output {
        &self.chars[index]
    }
}

impl From<&str> for FenderString {
    fn from(value: &str) -> FenderString {
        FenderString::new(value.chars().collect())
    }
}

impl PartialEq for FenderString {
    fn eq(&self, other: &Self) -> bool {
        self.chars == other.chars
    }
}

impl Hash for FenderString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.chars.hash(state);
    }
}

impl Display for FenderString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.has_changed.get() {
            self.has_changed.set(false);
            let mut cache = self.cached_string.borrow_mut();
            *cache = self.chars.iter().collect();
        }

        write!(f, "{}", self.cached_string.borrow())
    }
}
