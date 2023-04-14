use freight_vm::{error::FreightError, execution_engine::ExecutionEngine};
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use crate::fender_value::FenderValue;
use crate::{fender_reference::FenderReference, type_sys::type_system::FenderTypeSystem};

pub type NativeClosure =
    Rc<dyn Fn(&mut ExecutionEngine<FenderTypeSystem>) -> Result<FenderReference, FreightError>>;

#[derive(Clone)]
pub struct FenderIterator {
    pub next: NativeClosure,
    pub has_next: NativeClosure,
}

impl Debug for FenderIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Iterator")
    }
}

impl PartialEq for FenderIterator {
    fn eq(&self, other: &Self) -> bool {
        &*self.next as *const _ == &*other.next as *const _
    }
}

impl Into<FenderReference> for FenderIterator {
    fn into(self) -> FenderReference {
        FenderValue::Iterator(self).into()
    }
}

impl FenderIterator {
    pub fn new(len: usize, list: impl Fn(usize) -> FenderReference + 'static) -> FenderIterator {
        let i = Rc::new(RefCell::new(0));
        let i2 = i.clone();
        FenderIterator {
            next: Rc::new(move |_| {
                if *i.borrow() >= len {
                    Ok(Default::default())
                } else {
                    let mut borrow = i.borrow_mut();
                    let index = *borrow;
                    *borrow += 1;
                    Ok(list(index))
                }
            }),
            has_next: Rc::new(move |_| Ok(FenderValue::Bool(*i2.borrow() < len).into())),
        }
    }
}
