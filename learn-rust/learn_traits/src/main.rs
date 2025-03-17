#![allow(dead_code)]

pub trait Summary {
    fn summarize(&self) -> String;

    fn by(&self) -> String {
        // This is the default implementation
        String::from("anon")
    }
}

pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!("{}, by {} ({})", self.headline, self.author, self.location)
    }

    fn by(&self) -> String {
        self.author.clone()
    }
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

/*
If Summary had default implementations for all its methods and I wanted Tweet to
only use the default impls, I could've written this more ergonomically like this -
impl Summary for Tweet {}
*/
impl Summary for Tweet {
    fn summarize(&self) -> String {
        format!("{}: {}", self.username, self.content)
    }
}

fn structs_demo() {
    let tweet = Tweet {
        username: String::from("horse_ebooks"),
        content: String::from("of course, as you probably already know, people"),
        reply: false,
        retweet: false,
    };
    println!("1 new tweet {} - {}", tweet.summarize(), tweet.by());

    let article = NewsArticle {
        headline: String::from("AGI achieved!"),
        location: String::from("Silicon Valley"),
        author: String::from("OpenAI"),
        content: String::from("With GPT-42 we have finally achieved the elusive AGI."),
    };
    println!("1 new article {} - {}", article.summarize(), article.by());
}

/*
This way of declaring a function that takes in any type implementing the Summary
trait is a shortcut, the longcut way of declaring this function would be to use
generics -
pub fn notify<T: Summary>(item: T) {...}
*/
pub fn notify(item: &impl Summary) {
    println!("Breaking news! {}", item.summarize());
}

/*
In this function, item1 and item2 can be two different types as long as they
both implement Summary trait
*/
pub fn concat(item1: &impl Summary, item2: &impl Summary) -> String {
    format!("{} {}", item1.summarize(), item2.summarize())
}

// But here both the items have to be of the same type.
pub fn cat<T: Summary>(item1: &T, item2: &T) -> String {
    format!("{} - {}", item1.summarize(), item2.summarize())
}

/*
A simple function that returns some type that implements Summary. The caller
of this function will have no idea that the concrete type is actually Tweet.
The only thing the caller can do with the returned object is to call .summarize()
and nothing else.

If Rust does not know at compile which concrete type is being returned, then it
will result in a compile error. E.g., if the implementation of this function was
something like this -

if switch {
    Tweet {...}
} else {
    NewsArticle {...}
}

then I'd see a compile error.
*/
pub fn create_summarizable() -> impl Summary {
    Tweet {
        username: String::from("horse_ebooks"),
        content: String::from("of course, as you probably already know, people"),
        reply: false,
        retweet: false,
    }
}

fn function_demo() {
    let tweet = Tweet {
        username: String::from("horse_ebooks"),
        content: String::from("of course, as you probably already know, people"),
        reply: false,
        retweet: false,
    };

    let article = NewsArticle {
        headline: String::from("AGI achieved!"),
        location: String::from("Silicon Valley"),
        author: String::from("OpenAI"),
        content: String::from("With GPT-42 we have finally achieved the elusive AGI."),
    };

    notify(&tweet);
    notify(&article);

    println!("{}", concat(&tweet, &article));

    let news = NewsArticle {
        headline: String::from("Inside the crisis at Carta"),
        location: String::from("Silicon Valley"),
        author: String::from("The Information"),
        content: String::from("Carta co-founder Henry Ward once envisioned his CartaX—a secondary market brokerage service that would match smaller buyers and sellers of secondary stock—as the next Nasdaq."),
    };
    // cat(&article, &tweet); -> Both the items need to be of the same concrete type
    println!("{}", cat(&article, &news));

    let s = create_summarizable();
    // The following won't work, the only thing I can call on s is .summarize()
    // println!("{}", s.username);
    println!("{}", s.summarize());
}

fn main() {
    // structs_demo();
    function_demo();
}
