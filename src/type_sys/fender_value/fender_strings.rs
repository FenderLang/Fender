use std::{cell::RefCell, fmt::Display, hash::Hash, ops::Index};

use crate::index_oob;

#[derive(Debug, Clone)]
pub struct FenderString {
    chars: Vec<char>,
    cacheable: bool,
    cached_string: RefCell<Option<String>>,
}

impl FenderString {
    pub fn new(chars: Vec<char>) -> FenderString {
        FenderString {
            chars,
            cacheable: false,
            cached_string: Default::default(),
        }
    }

    pub fn new_cached(chars: Vec<char>) -> FenderString {
        FenderString {
            chars,
            cacheable: true,
            cached_string: Default::default(),
        }
    }

    pub fn not_cached_from_str(str_val: &str) -> FenderString {
        FenderString {
            chars: str_val.chars().collect(),
            cacheable: false,
            cached_string: RefCell::new(None),
        }
    }
    pub fn cached_from_str<S: ToString>(str_val: S) -> FenderString {
        let str_val = str_val.to_string();
        FenderString {
            chars: str_val.chars().collect(),
            cacheable: true,
            cached_string: RefCell::new(Some(str_val)),
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

        if let Some(s) = self.cached_string.borrow_mut().as_mut() {
            s.push_str(other);
        }
    }

    pub fn push_fndr_str(&mut self, other: &FenderString) {
        if other.is_empty() {
            return;
        }

        self.chars.extend(other.chars.clone());

        if let Some(s) = self.cached_string.borrow_mut().as_mut() {
            for c in other.iter() {
                s.push(*c);
            }
        }
    }

    pub fn push_char(&mut self, other: char) {
        self.chars.push(other);

        if let Some(s) = self.cached_string.borrow_mut().as_mut() {
            s.push(other);
        }
    }

    pub fn remove(&mut self, pos: usize) -> Option<char> {
        if pos >= self.len() {
            None
        } else {
            if self.cacheable {
                *self.cached_string.get_mut() = None;
            }
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
            if self.cacheable {
                *self.cached_string.get_mut() = None;
            }
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
        if self.cacheable {
            *self.cached_string.get_mut() = None;
        }
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
                if self.cacheable {
                    *self.cached_string.get_mut() = None;
                }
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
                if self.cacheable {
                    *self.cached_string.get_mut() = None;
                }
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
            cached_string: RefCell::new(None),
            chars: value.chars().collect(),
            cacheable: false,
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
        if !self.cacheable {
            return write!(f, "{}", self.chars.iter().collect::<String>());
        }

        if let Some(s) = &*self.cached_string.borrow() {
            write!(f, "{}", s)
        } else {
            *self.cached_string.borrow_mut() = Some(self.chars.iter().collect());
            write!(f, "{}", self.cached_string.borrow().as_ref().unwrap())
        }
    }
}
