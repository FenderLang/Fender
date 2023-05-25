use std::{
    cell::{Cell, RefCell},
    fmt::Display,
    hash::Hash,
    ops::Index,
};

use crate::index_oob;

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

        self.chars.extend(other.chars());

        if !self.has_changed.get() {
            self.cached_string.borrow_mut().push_str(other);
        }
    }

    pub fn push_fndr_str(&mut self, other: &FenderString) {
        if other.is_empty() {
            return;
        }

        self.chars.extend(other.chars.clone());

        if !self.has_changed.get() {
            if other.has_changed.get() {
                self.has_changed.set(true);
            } else {
                self.cached_string
                    .borrow_mut()
                    .push_str(other.cached_string.borrow().as_str());
            }
        }
    }

    pub fn push_char(&mut self, other: char) {
        self.chars.push(other);
        if !self.has_changed.get() {
            self.cached_string.borrow_mut().push(other);
        }
    }

    pub fn remove(&mut self, pos: usize) -> Option<char> {
        if pos >= self.len() {
            None
        } else {
            self.has_changed.set(true);
            Some(self.chars.remove(pos))
        }
    }

    pub fn remove_at(&mut self, mut pos: i64) -> Result<char, String> {
        pos = if pos < 0 {
            self.len() as i64 + pos
        } else {
            pos
        };

        let pos = if pos < 0 {
            return Err("Invalid wrapping index".into());
        } else {
            pos as usize
        };

        if pos >= self.len() {
            Err(index_oob!(pos, self.len()))
        } else {
            self.has_changed.set(true);
            Ok(self.chars.remove(pos))
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

    pub fn swap(&mut self, pos_a: usize, pos_b: usize) -> bool {
        if pos_a >= self.len() || pos_b >= self.len() {
            return false;
        }
        self.has_changed.set(true);
        self.chars.swap(pos_a, pos_b);
        true
    }

    pub fn insert_char(&mut self, index: usize, value: char) -> bool {
        match index.cmp(&self.len()) {
            std::cmp::Ordering::Greater => false,
            std::cmp::Ordering::Equal => {
                self.push_char(value);
                true
            }
            _ => {
                self.has_changed.set(true);
                self.chars.insert(index, value);
                true
            }
        }
    }

    pub fn insert_str(&mut self, index: usize, value: &str) -> bool {
        match index.cmp(&self.len()) {
            std::cmp::Ordering::Greater => false,
            std::cmp::Ordering::Equal => {
                self.push_str(value);
                true
            }
            _ => {
                self.has_changed.set(true);
                let mut tmp = self.chars.split_off(index);
                self.chars.extend(value.chars());
                self.chars.append(&mut tmp);
                true
            }
        }
    }

    pub fn get(&self, index: usize) -> Option<char> {
        self.chars.get(index).cloned()
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
        FenderString {
            has_changed: Cell::new(false),
            cached_string: RefCell::new(value.to_owned()),
            chars: value.chars().collect(),
        }
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
