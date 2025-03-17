#![allow(dead_code)]
#![allow(unused_variables)]

mod cookie;

fn main() {
    // println!("learn_mut_ref_1:");
    // learn_mut_ref_1();

    // println!("\nunexplained_1:");
    // unexplained_1();

    // println!("\nunexplained_2:");
    // unexplained_2();
    let c = add(2, 3);
    println!("{c}");
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn learn_mut_ref_1() {
    let mut s = String::from("Hello");
    let p1 = &mut s;
    println!("{p1}");
    let p2 = &mut s;
    println!("{p2}");
    /*
    Using p2 after declaring and using p1 is fine, as long as I never use p1 ever
    again. If I uncomment either of the two lines below, the borrow checker will
    complain that s cannot have multiple mutable references.
    From the rust book:
    > Note that a reference’s scope starts from where it is introduced and
    > continues through the last time that reference is used.
     */
    // p1.push_str(", World!");
    // println!("{p1}");
}

fn unexplained_2() {
    let mut s = String::from("Hello");
    let p1 = &mut s;
    println!("{p1}");
    let p2 = &p1; // Is this a loop hole in the borrow checker?
    println!("{p2}");
    // println!("{p1}");
    // p1.push_str(", World!");
    // println!("{p1}");
    println!("{p2}");
}

fn unexplained_1() {
    let mut s = String::from("Hello");
    let p1 = &mut s;
    /*
    Just ending this function with a println!("{s}") works fine. But if I follow
    this with using (printing) p1, borrow checker complains that I cannot use s.
    However, if I use s after using p1, everything is fine.
     */
}
