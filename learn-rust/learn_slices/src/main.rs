#![allow(dead_code)]

/*
A String object has the actual data, the array of characters, on the heap. It has
an accompanying structure in the stack which has a pointer to the heap, the
length, and other attributes. If I try to return a borrowed reference from a
function Rust will complain because the original structure on the stack still
retains ownership and it will be destroyed at the end of the function. The data
on the heap will become "orphaned". Same holds for a Vec<T>.

My initial assumption is that an array like [i32] probably lives on the stack
directly so it is obvious why I cannot return borrowed references to it. The
data itself will be lost when the function goes out of scope. But it does not
explain how I can return a slice from it. Maybe when I am slicing it Rust puts
it on the heap? Or maybe the array also lives on the heap with an accompanying
structure with attributes on the stack, just like any other object.

Return type is
  * &String
  * &str

Returning type is
  * &String::from("content")
  * ans = String::from("content"); &ans

*/

fn bad_func_1(x: &str) -> &str {
    // let ans = String::from("Too long Too short");
    // let ans = &String::from("Too long Too short")[..];
    let ans = "Too long Too short";
    if x.len() > 10 {
        &ans[..8]
    } else {
        &ans[8..]
    }
}

// fn bad_func_2(x: &[i32]) -> &[i32] {
//     // let ans = [10, 11, 12, 13, 14, 15, 16, 17, 18, 19];
//     // let ans = vec![10, 11, 12, 13, 14, 15, 16, 17, 18, 19];
//     // let ans = &vec![10, 11, 12, 13, 14, 15, 16, 17, 18, 19][..];
//     let ans = &[10, 11, 12, 13, 14, 15, 16, 17, 18, 19][..];
//     if x.len() > 10 {
//         &ans[..5]
//     } else {
//         &ans[5..]
//     }
// }

fn main() {
    // func1_demo();
}
