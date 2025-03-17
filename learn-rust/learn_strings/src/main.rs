/*
The len method gives the number of bytes that are used to store the string. All
strings are stored in the utf-8 encoding. the .chars() method will iterate over
the glyphs that are nearest in concept to characters.

A &String can be coereced to &str, but the reverse is not true, i.e., if there
is a function that accepts &String and I try to pass it &str, then it will not
compile.
*/
fn len(s: &String) -> usize {
    s.len()
}

fn length(s: &str) -> usize {
    s.chars().count()
}

fn main() {
    let s = String::from("नमस्ते");
    println!("{}", len(&s)); // &String -> &String works
    println!("{}", length(&s)); // &String -> &str works

    let s1 = "धन्यवाद";
    // println!("{}", len(s1)); --> &str -> &String does not work
    println!("{}", len(&s1.to_string()));
    println!("{}", length(s1)); // &str -> &str works
}
