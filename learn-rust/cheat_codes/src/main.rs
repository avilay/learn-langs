#![allow(dead_code)]

use std::{fmt::Display, io};

// Syntax cheat for lifetimes and generics
fn longest<'a, T>(x: &'a str, y: &'a str, msg: T) -> &'a str
where
    T: Display,
{
    println!("Announcement: {msg}");
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

// Get user input
fn input() {
    let mut enter = String::new();
    io::stdin()
        .read_line(&mut enter)
        .expect("Failed to read line!");
}

fn main() {
    println!("Hello, world!");
}
