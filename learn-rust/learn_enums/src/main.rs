/*
Enums are weirdly different in Rust. They are a full-fledged type that can have
different ctors and even methods. The ctor for each variant is named the same as
the variant itself, i.e., to create a Write message I can do Message::Write(...)
*/
#![allow(dead_code)]
use rand::Rng;
#[derive(Debug)]
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

impl Message {
    fn display(&self) -> String {
        match self {
            Message::Quit => String::from("[X]"),
            Message::Move { x, y } => String::from(format!("Moving to {x}, {y}")),
            Message::Write(msg) => String::from(msg),
            Message::ChangeColor(r, g, b) => String::from(format!("RGB({r}, {g}, {b})")),
        }
    }
}

fn msg_demo() {
    let quit = Message::Quit;
    let msg = quit.display();
    println!("{msg}");

    let mov = Message::Move { x: 10, y: 10 };
    let msg = mov.display();
    println!("{msg}");

    let write = Message::Write(String::from("Write something"));
    let msg = write.display();
    println!("{msg}");

    let color = Message::ChangeColor(128, 23, 243);
    let msg = color.display();
    println!("{msg}");
}

fn match_demo() {
    /*
    Pattern matching is usually coupled with enums but it can be used with any
    type. The match "arms" have to be exhaustive, i.e., match block should
    address all possible outcomes, but we can use placeholders as "default" action.
     */
    let dice_roll = 9;
    match dice_roll {
        3 => add_fancy_hat(),
        7 => remove_fancy_hat(),
        _ => (),
    }

    fn add_fancy_hat() {}
    fn remove_fancy_hat() {}
}

fn increment(x: Option<i32>) -> i32 {
    match x {
        Some(v) => v + 1,
        None => 0,
    }
}

fn random() -> Option<i32> {
    let mut rng = rand::thread_rng();
    let x = rng.gen_range(1..=100);
    if x > 50 {
        Some(x)
    } else {
        None
    }
}

/*
This function gets a random number and increments it. If the random number is
None, it also returns None, otherwise it will add 1 to the result and return
it in Some.

let n = match random() {
    Some(num) => num,
    None => return None,
};
Some(n + 1)

But there is a more "ergonomic" way to write this -

let n = random()?
n + 1

The ? at the end of random will result in the value in Some if it is there. If
random returned a None, ? will also return None from the function. Any code after
that is not executed.

Of course this can be written even more succintly as -

Some(random()? + 1)

*/
fn increment_random() -> Option<i32> {
    Some(random()? + 1)
}

fn option_demo() {
    /*
    Option is a prelude enum that looks like -
    enum Option<T> {
        Some(T),
        None
    }
    We don't have to use Option::Some or Option::None, we can just use Some
    and None directly.
     */
    let x = Some(41);
    let a = increment(x);
    println!("{a}");

    let y: Option<i32> = None;
    let b = increment(y);
    println!("{b}");

    if let Some(x) = random() {
        println!("Got {x} back");
    }

    let c = random();
    if let Some(x) = c {
        println!("Got {x} back");
    }

    let n = increment_random().unwrap_or_default();
    println!("Value of increment_random = {n}");
}

fn main() {
    // msg_demo();
    // match_demo();
    option_demo();
}
