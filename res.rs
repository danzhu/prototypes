use std::ops;

use Res::*;

enum Res<T, F> {
    T(T),
    F(F),
}

impl<T, F> Res<T, F> {
    fn t(self) -> bool {
        match self {
            T(_) => true,
            F(_) => false,
        }
    }
}

impl<T, F> ops::BitAnd for Res<T, F> {
    type Output = Res<T, F>;

    fn bitand(self, rhs: Res<T, F>) -> Res<T, F> {
        match (self, rhs) {
            (T(t), T(_)) => T(t),
            (T(_), F(f)) => F(f),
            (F(f), _) => F(f),
        }
    }
}

fn main() {}
