use std::{cell::UnsafeCell, fmt::Debug, ops::Deref, ops::DerefMut, rc::Rc};

#[derive(Clone)]
pub struct InternalReference<T>(Rc<UnsafeCell<T>>);

impl<T> InternalReference<T> {
    pub fn new(value: T) -> Self {
        Self(Rc::new(UnsafeCell::new(value)))
    }
}

impl<T: Debug> Debug for InternalReference<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternalReference({:?})", **self)
    }
}

impl<T> Deref for InternalReference<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0.get() }
    }
}

impl<T> DerefMut for InternalReference<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.get().as_mut().unwrap() }
    }
}

impl<T: PartialEq> PartialEq for InternalReference<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: PartialOrd> PartialOrd for InternalReference<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T> From<T> for InternalReference<T> {
    fn from(value: T) -> Self {
        InternalReference::new(value)
    }
}
