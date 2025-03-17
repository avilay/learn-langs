#![allow(dead_code)]
#[derive(Debug)]
struct Rectangle {
    height: u32,
    width: u32,
}

impl Rectangle {
    // This is a "static" method, it is called an associated funciton in Rust.
    fn square(side: u32) -> Self {
        Rectangle {
            height: side,
            width: side,
        }
    }

    /*
    In the signature for area, we use &self instead of rectangle: &Rectangle.
    The &self is actually short for self: &Self. Within an impl block, the type
    Self is an alias for the type that the impl block is for. Methods must have
    a parameter named self of type Self for their first parameter, so Rust lets
    you abbreviate this with only the name self in the first parameter spot.
    Note that we still need to use the & in front of the self shorthand to
    indicate that this method borrows the Self instance, just as we did in
    rectangle: &Rectangle. Methods can take ownership of self, borrow self
    immutably, as we’ve done here, or borrow self mutably, just as they can any
    other parameter.
     */
    fn area(&self) -> u32 {
        return self.height * self.width;
    }
}

#[derive(Debug)]
struct User {
    is_active: bool,
    num_signins: u32,
    username: String,
    email: String,
}

fn ownership_demo() {
    let happy = User {
        is_active: true,
        num_signins: 10,
        username: String::from("Happy Orange"),
        email: String::from("happy@orange.com"),
    };

    dbg!(&happy);

    let cookie = User {
        username: String::from("Cookie Monster"),
        email: String::from("cookie@monster.com"),
        ..happy // copy everything else from happy
    };

    // is_active and num_signins are copy-able, so they were copied and both
    // happy and cookie are accessible.
    println!(
        "Both cookie and happy are valid -\n{:?}\n{:?}",
        happy, cookie
    );

    // username and email are not copy-able, so they had to be moved, resulting
    // in the entire struct being moved. happy is no longer accessible.
    let happy_again = User {
        is_active: false,
        ..happy
    };
    println!("Only happy_again is valid -\n{:?}", happy_again);
    // println!("{:?}", happy); --> will throw a compile error
}

fn method_demo() {
    let sq = Rectangle::square(10);
    /*
    area borrows a reference to the sq object, strictly speaking I should
    be calling it like this - (&sq).area(), similar to C++ syntax sq->area()
    or (*sq).area() but Rust does automatic referencing and dereferencing
     */
    let a = sq.area();
    println!("{:?} has area {a}", sq);
}

fn main() {
    // method_demo();
    ownership_demo();
}
