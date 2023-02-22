use std::sync::{RwLock, RwLockReadGuard};

pub struct LazyCell<T: Send + Sync> {
    data: RwLock<Option<T>>,
    init: fn() -> T,
}

impl<T: Send + Sync> LazyCell<T> {
    pub const fn new(init: fn() -> T) -> LazyCell<T> {
        LazyCell {
            data: RwLock::new(None),
            init,
        }
    }

    pub fn get(&self) -> RwLockReadGuard<Option<T>> {
        let read = self.data.read().unwrap();
        if read.is_none() {
            drop(read);
            let mut write = self.data.write().unwrap();
            *write = Some((self.init)());
        }
        self.data.read().unwrap()
    }
}
