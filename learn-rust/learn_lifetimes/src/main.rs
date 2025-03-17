/*
Lifetimes are the programmer telling Rust how a function's input parameters are
related to each other and the rest of the program. Rust has certain rules where
it can infer the programmers' intent and the lifetime specs can be omitted. These
are called Elision Rules, they are -
  1. A separate lifetime is automatically assigned to each parameter.
  2. If there is exactly one input parmaeter, its lifetime is assigned to all
     outputs.
  3. If there are multiple input parameters but one of them is &self or &mut self
     then the lifetime of self is assigned to all outputs.

static is a special lifetime that can live for the entire duration of the program.
String literals (aka &str) have static lifetime by default, i.e., they are baked
into the binary.

Syntax -
&'a i32 - this is borrowed reference to an integer with a lifetime of 'a
&'a mut i32 - this is a borrowed reference to a mutable integer with a lifetime of 'a

Structs can also have members that are references as long as their lifetimes are
specified.
*/

#![allow(dead_code)]

// This is saying that the output of longest is same as the smaller of the
// of the referenced values.
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

fn longest_demo() {
    let name1 = String::from("Avilay");
    let name2 = String::from("Don Avilyoni");
    let name = longest(name1.as_str(), name2.as_str());
    println!("{name}");
}

fn randint() -> i32 {
    // placeholder for a real random number generator
    42
}

// This is saying that the function returns a string literal
fn greet() -> &'static str {
    if randint() > 50 {
        "Hello"
    } else {
        "नमस्ते"
    }
}

fn greet_demo() {
    let greeting = greet();
    println!("{greeting}");
}

// Here Rust Elison Rules mistakenly infer that the output will have the same
// lifetime as the input x, when in reality the output has a static lifetime.
// Rust is erring on the side of caution, so it is acceptable.
fn fool(x: &str) -> &str {
    if x.len() > 5 {
        "Too long"
    } else {
        "Too short"
    }
}

fn fool_demo() {
    println!("{}", fool("Hello"));
}

// This is saying that the instantiated struct object should have the same lifetime
// as the reference that it is instantiated with.
#[derive(Debug)]
struct ImportantExcerpt<'a> {
    part: &'a str,
}

impl<'a> ImportantExcerpt<'a> {
    fn level(&self) -> i32 {
        3
    }
}

fn struct_demo() {
    let novel = String::from("Call me Ishmael. Some years ago...");
    let first_sentence = novel.split('.').next().expect("Could not find a '.'");
    let excerpt = ImportantExcerpt {
        part: &first_sentence,
    };
    // excerpt's lifetime can only be as long as first_setence's.
    println!("{:?}", excerpt);
}

fn main() {
    // longest_demo();
    // greet_demo();
    // fool_demo();
    struct_demo();
}
