#![allow(dead_code)]

use std::{cmp, fmt};

/*
Pretty standard stuff here. This is a generic function with the generic data type
having an upper bound on PartialOrd trait, i.e., T can be any type that implements
this trait.
*/
fn largest<T: cmp::PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];

    for item in list {
        if item > largest {
            largest = item;
        }
    }

    largest
}

/*
Here the generic type has two different upper bounds. Using where is just a less
verbose way of declaring the function like this -
fn notify<T: fmt::Display + Clone, U: Clone + fmt::Debug>(t: &T, u: &U) {
*/
fn notify<T, U>(t: &T, u: &U)
where
    T: fmt::Display + Clone,
    U: fmt::Debug + Clone,
{
    let newt = t.clone();
    let newu = u.clone();
    println!("{newt}");
    dbg!(newu);
}

#[derive(Debug, Clone)]
struct Cookie {
    flavor: String,
    calories: u32,
}

fn function_generics_demo() {
    let number_list = vec![34, 50, 25, 100, 65];

    let result = largest(&number_list);
    println!("The largest number is {}", result);

    let char_list = vec!['y', 'm', 'a', 'q'];

    let result = largest(&char_list);
    println!("The largest char is {}", result);

    let cookie = Cookie {
        flavor: String::from("Chocolate Chip"),
        calories: 200,
    };
    let msg = String::from("Hello World");
    notify(&msg, &cookie);
}

// Also pretty standard, this struct is made of a generic data type with no constraints.
struct Point<T> {
    x: T,
    y: T,
}

// All impls have to be specifythe generic type, even if the method does not
// make any use of it.
impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }

    fn foo(&self) {
        println!("hahaha");
    }
}

// I can also implement methods that are only valid on a specific datatype
impl Point<f32> {
    fn distance_from_origin(&self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}

fn struct_generic_demo() {
    let p = Point { x: 1.0, y: 1.0 };
    p.foo();
    println!("{}", p.x);
    // The following will not work because p is Point<f64> and distance_from_origin
    // is only defined for Point<f32>
    // println!("{}", p.distance_from_origin())

    let p2: Point<f32> = Point { x: 1., y: 2. };
    println!("{}", p2.distance_from_origin());
}

fn main() {
    function_generics_demo();
    // struct_generic_demo();
}
