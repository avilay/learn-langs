#![allow(dead_code)]

use rand::Rng;
use std::cmp::Ordering;
use std::io;

fn main_1() {
    println!("Guess the number!");

    let secret_number = rand::thread_rng().gen_range(1..=100);

    println!("Please input your guess.");
    let mut guess = String::new();
    io::stdin()
        .read_line(&mut guess)
        .expect("Failed to read line.");
    let guess_number: i32 = guess
        .trim()
        .parse()
        .expect("{guess} is not a valid number! {e}");

    if guess_number == secret_number {
        println!("Correct guess: {guess_number}");
    } else {
        println!("Incorrect guess: {guess_number}. Secret: {secret_number}");
    }
}

fn main_2() {
    println!("Guess the number!");

    let secret_number = rand::thread_rng().gen_range(1..=100);

    let guess_number: i32 = loop {
        println!("Please input your guess.");
        let mut guess = String::new();
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read line.");
        match guess.trim().parse() {
            Ok(v) => break v,
            Err(_) => println!("{guess} is not a valid number. Please try again."),
        }
    };

    if guess_number == secret_number {
        println!("Correct guess: {guess_number}");
    } else {
        println!("Incorrect guess: {guess_number}. Secret: {secret_number}");
    }
}

fn main_3() {
    println!("Guess the number!");

    let secret_number = rand::thread_rng().gen_range(1..=100);

    let guess_number: i32 = loop {
        println!("Please input your guess.");
        let mut guess = String::new();
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read line.");
        match guess.trim().parse() {
            Ok(v) => break v,
            Err(_) => println!("{guess} is not a valid number. Please try again."),
        }
    };

    println!("You guessed: {guess_number}");
    match guess_number.cmp(&secret_number) {
        Ordering::Less => println!("Too small!"),
        Ordering::Greater => println!("Too big!"),
        Ordering::Equal => println!("You win!"),
    }
    println!("Secret number: {secret_number}");
}

fn read_guess() -> i32 {
    loop {
        println!("Please input your guess.");
        let mut guess = String::new();
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read line.");
        match guess.trim().parse() {
            Ok(v) => break v,
            Err(_) => println!("{guess} is not a valid number. Please try again."),
        }
    }
}

fn check_guess(guess: i32, actual: i32) -> bool {
    match guess.cmp(&actual) {
        Ordering::Less => {
            println!("Too small!");
            return false;
        }
        Ordering::Greater => {
            println!("Too big!");
            return false;
        }
        Ordering::Equal => {
            println!("You win!");
            return true;
        }
    }
}

fn main_4() {
    println!("Guess the number!");

    let secret_number = rand::thread_rng().gen_range(1..=100);
    println!("Cheat: {secret_number}");

    for attempt in 1..=3 {
        println!("\n### Attempt {attempt} ###");
        let guess_number: i32 = read_guess();

        println!("You guessed: {guess_number}");
        if check_guess(guess_number, secret_number) {
            break;
        }
    }

    println!("Secret number: {secret_number}");
}

fn main_5() {
    println!("Guess the number!");

    let secret_number = rand::thread_rng().gen_range(1..=100);

    loop {
        println!("Please input your guess");

        let mut guess = String::new();
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read line!");

        let guess: &str = guess.trim();
        if guess == "quit" {
            break;
        }

        let guess: u32 = match guess.parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        println!("You guessed: {guess}");

        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal => {
                println!("You win!");
                break;
            }
        }
    }
}

fn main() {
    // main_1();
    // main_2();
    // main_3();
    // main_4();
    main_5();
}
