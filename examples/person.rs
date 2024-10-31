#![allow(dead_code)]

mod inner {
    use typestate_builder::TypestateBuilder;

    #[derive(Debug, TypestateBuilder)] // `Debug` is not a must.
    pub(super) struct Person {
        #[typestate_builder(default)]
        name: String,
        age: u32,
        email: Option<String>,
    }
}

fn main() {
    let person = inner::Person::builder()
        .name("Alice Johnson".to_string())
        .age(30)
        .email(Some("alice@example.com".to_string()))
        .build();

    println!("Created person: {:?}", person);
}
