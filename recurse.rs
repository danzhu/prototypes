use std::rc::Rc;

struct Rec<T> {
    f: Rc<Fn(Rec<T>) -> T>,
}

impl<T> Rec<T> {
    fn recurse(&self) -> T {
        (self.f)(self.clone())
    }
}

impl<T> Clone for Rec<T> {
    fn clone(&self) -> Self {
        Rec { f: self.f.clone() }
    }
}

fn recursive<T, F>(f: F) -> T
where
    F: Fn(Rec<T>) -> T + 'static,
{
    let f = Rc::new(f);
    f(Rec { f: f.clone() })
}

fn main() {
    let fac = recursive(|this: Rec<Rc<Fn(i32) -> i32>>| {
        Rc::new(move |n| match n {
            1 => 1,
            _ => {
                let fac = this.recurse();
                n * fac(n - 1)
            }
        })
    });
    let (even, odd) = recursive(|this: Rec<(Rc<Fn(i32) -> bool>, Rc<Fn(i32) -> bool>)>| {
        let this2 = this.clone();
        let even = Rc::new(move |n| {
            n == 0 || {
                let (_, odd) = this.recurse();
                odd(n - 1)
            }
        });
        let odd = Rc::new(move |n| {
            n != 0 && {
                let (even, _) = this2.recurse();
                even(n - 1)
            }
        });
        (even, odd)
    });
    // let undef = recursive(|this: Rec<i32>| this.recurse());

    println!("{}", fac(4));
    println!("{} {}", even(5), odd(5));
}
