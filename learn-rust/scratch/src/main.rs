// fn foo() -> &str {
//     let name = "Avilay Parekh";
//     &name[..6]
// }

fn foo(x: &str) -> &str {
    let ans = "Too long Too short";
    if x.len() > 10 { &ans[..8] } else { &ans[8..] }
}

fn main() {
    let name = String::from("Avilay Parekh");
    let ans = foo(&name);
    println!("{ans}");
    println!("{name}");
}
