#![allow(dead_code)]
use std::fs::File;
use std::io;
use std::io::ErrorKind;
use std::io::Read;

fn level2() {
    panic!("KA-BOOM!");
}

fn level1() {
    level2();
}

/*
Rust does not have throwable exceptions. Instead we can return an enum Result<T, E>
where T is the return type and E is the error type.

pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

There is an Error trait which I can use to implement my own error. std::io::Error
is one of the error structs that implements the Error trait.

A very common error handling pattern is to do something useful with the result
or panic if there is an error.

let fd = match File::open("hello.txt") {
    Ok(file) => file,
    Err(error) => panic!("KA-BOOM! Cannot open hello.txt!"),
};

Rust has a shortcut for this using the Result.unwrap method which panics if there
is an Err, otherwise will return what is inside Ok.

let _fd = File::open("hello.txt").unwrap();

The only difference from the match way of doing things is that I cannot provide
my own custom error message with .unwrap. To do this I can use .expect() which
is similar in that it will panic if there is an Err, but I can provide my own
error message.
 */
fn error_handling_demo_1() {
    let _fd = File::open("hello.txt").expect("Unable to open hello.txt");
}

/*
Another common pattern is to handle different error types differently. The file
IO api returns std::io::Error in Err. This Error struct has a method called .kind
that returns the ErrorKind enum. I can use pattern matching on this enum for
custom error handling.

let _fd = match File::open("./some/path/hello.txt") {
    Ok(file) => file,
    Err(error) => match error.kind() {
        ErrorKind::NotFound => match File::create("./some/path/hello.txt") {
            Ok(fc) => fc,
            Err(_err) => panic!("Unable to create hello.txt!"),
        },
        _ => panic!("Unable to open hello.txt!"),
    },
};

Instead of these nested matches, it is better to use lambda functions using the
Result.unwrap_or_else() method.
*/
fn error_handling_demo_2() {
    let _fd = File::open("./some/path/hello.txt").unwrap_or_else(|err| {
        if err.kind() == ErrorKind::NotFound {
            File::create("./some/path/hello.txt").expect("Unable to create ./some/path.hello.txt")
        } else {
            panic!("Unable to open ./some/path/hello.txt");
        }
    });
}

/*
A common pattern for error propagation from a function when calling another function
that can return an Err is to use the success value inside Ok and continue on, but
if the underlying function returned an Err, just return that and stop.

let mut fd = match File::open("username.txt") {
    Ok(file) => file,
    Err(error) => return Err(error),
};
let mut username = String::new();
match fd.read_to_string(&mut username) {
    Ok(contents) => contents,
    Err(error) => return Err(error),
};
Ok(username)

A more ergonomic way of writing this is to use the ? operator. This will result
in the value inside Ok if it is there, but return from the function if the result
is Err.

Of course in this particular case, an even shorter way of writing this is to use
the fs::read_to_string() function.
*/
fn error_propagation_demo() -> Result<String, io::Error> {
    let mut username = String::new();
    File::open("username.txt")?.read_to_string(&mut username)?;
    Ok(username)
}

fn main() {
    // level1();
    // error_handling_demo_1();
    // error_handling_demo_2();
    println!("{}", error_propagation_demo().unwrap());
}
