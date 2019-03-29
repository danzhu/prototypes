use std::cell::UnsafeCell;
use std::{ops, ptr};

struct Lazy<F, T> {
    state: UnsafeCell<LazyState<F, T>>,
}

enum LazyState<F, T> {
    Calc(F),
    Value(T),
}

impl<F, T> Lazy<F, T>
where
    F: FnOnce() -> T,
{
    fn new(f: F) -> Self {
        Lazy {
            state: UnsafeCell::new(LazyState::Calc(f)),
        }
    }
}

impl<F, T> ops::Deref for Lazy<F, T>
where
    F: FnOnce() -> T,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        use LazyState::*;

        unsafe {
            let state = self.state.get();
            match &*state {
                Calc(f) => {
                    let f = ptr::read(f);
                    ptr::write(state, Value(f()));
                    self.deref()
                }
                Value(v) => v,
            }
        }
    }
}

fn main() {
    let mut v = vec![1];
    let l = Lazy::new(|| {
        v.push(2);
        v
    });
    println!("l = {:?}", *l);
    println!("l = {:?}", *l);
}
