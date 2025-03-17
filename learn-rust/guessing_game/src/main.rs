use rand::Rng;
use std::{cmp::Ordering, io};

fn main() {
    println!("**** GUESS THE NUMBER ****");
    let secret_number = rand::thread_rng().gen_range(1..=100);

    loop {
        println!("Enter your guess: ");
        let mut guess = String::new();
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read line!");

        // let guess: u32 = guess.trim().parse().expect("Please enter a valid number.");

        if guess.trim() == "quit" {
            break;
        }

        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        println!("You guessed: {guess}");

        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small!"),
            Ordering::Equal => {
                println!("You win!");
                break;
            }
            Ordering::Greater => println!("Too big!"),
        }
    }
}
