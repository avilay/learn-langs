#![allow(dead_code)]

use std::collections::HashMap;

fn dict_demo() {
    // No need to specify the type, Rust will figure it out after my first insert
    let mut dict = HashMap::new();
    dict.insert("one".to_string(), 1);
    dict.insert("two".to_string(), 2);
    let key = String::from("one");
    // I could've written the following three lines as -
    // let score = dict.get(&key).copied().unwrap_or(0);
    let score = dict.get(&key);
    let score = score.copied();
    let score = score.unwrap_or(0);
    println!("{score}");

    // This will be unordered.
    for (key, value) in &dict {
        println!("{key}: {value}");
    }

    // by default .insert will overwrite existing values
    dict.insert("one".to_string(), 100);
    println!("{:?}", dict);

    // I can do conditional insert, where new value is inserted only if the key
    // did not exist - the below code is equivalent to -
    // if key not in dict:
    //   dict[key] = val
    dict.entry("one".to_string()).or_insert(1);
    dict.entry("three".to_string()).or_insert(3);
    println!("{:?}", dict);

    // I can use the fact that or_insert returns a mutable reference to the value
    // to update existing values
    let mut hsh = HashMap::new();
    hsh.insert("units".to_string(), vec![1, 2, 3]);
    hsh.insert("tens".to_string(), vec![10, 20, 30]);
    let units = hsh.entry("units".to_string()).or_insert(vec![]);
    units.push(4);
    println!("{:?}", hsh);

    // Another example where the value is copy value
    let one = dict.entry("one".to_string()).or_insert(1);
    *one = 1000;
    println!("{:?}", dict);
}

fn vectors_demo() {
    let mut cookies = vec![
        String::from("Chocolate Chip"),
        String::from("Snicker Doddle"),
    ];
    let calories = vec![200, 220];

    /*
    The below code will not compile because it is trying to move the string value
    at 0th index to another variable called cc. But this would render the original
    vector useless. So this is not allowed. Instead I have to borrow the value.
     */
    // let cc = cookies[0];
    let cc = &cookies[0];

    /*
    This will work because it copies the integer value at 0th index.
     */
    let cal = calories[0];

    println!("This yummy {cc} cookie has {cal} calories.");

    cookies.push(String::from("Oatmeal Raisin"));
    println!("{:?}", cookies);

    let oatr = cookies.get(2);
    match oatr {
        Some(cookie) => println!("Got cookie {cookie}"),
        None => println!("No cookie for you!"),
    }

    if let Some(cookie) = cookies.get(20) {
        println!("This code is unreachable {cookie}");
    }

    // for will move v and v cannot be used ever after for is done with it
    let v = vec![1, 2, 3];
    for x in v {
        println!("{x}");
    }
    // println!("{:?}", v);

    // for is borrowing u, so everything is fine
    let u = vec![1, 2, 3];
    for x in &u {
        println!("{x}");
    }
    println!("{:?}", u);

    // I can even change the elements while iterating over them using the special
    // deference operator *
    let mut w = vec![1, 2, 3];
    for x in &mut w {
        *x += 10;
    }
    println!("{:?}", w);

    // To get around the limitation of uniform datatypes in vector I can store
    // enum in a vector
    #[derive(Debug)]
    enum Feature {
        Categorical(String),
        Numeric(f32),
    }
    let features = vec![
        Feature::Categorical(String::from("True")),
        Feature::Numeric(3.2),
    ];
    println!("{:?}", features);

    println!("Done");
}

fn main() {
    // vectors_demo();
    dict_demo();
}
