mod inner {
    use typestate_builder::TypestateBuilder;

    #[derive(Debug /* TypestateBuilder */)] // `Debug` is not a must.
    pub(super) struct Person {
        name: String,
        age: u32,
        email: Option<String>,
    }
    pub(super) trait PersonBuilderFieldBuild {
        type Built;
        fn build(self) -> Self::Built;
    }
    pub(super) struct PersonBuilder<NameGenericParam, AgeGenericParam, EmailGenericParam>
    where
        NameGenericParam: PersonBuilderFieldBuild,
    {
        name: NameGenericParam,
        age: AgeGenericParam,
        email: EmailGenericParam,
    }
    pub(super) struct PersonBuilderNameEmpty;
    impl PersonBuilderFieldBuild for PersonBuilderNameEmpty {
        type Built = String;
        fn build(self) -> Self::Built {
            String::default()
        }
    }
    pub(super) struct PersonBuilderNameAdded(String);
    impl PersonBuilderFieldBuild for PersonBuilderNameAdded {
        type Built = String;
        fn build(self) -> Self::Built {
            self.0
        }
    }
    pub(super) struct PersonBuilderAgeEmpty;
    pub(super) struct PersonBuilderAgeAdded(u32);
    impl PersonBuilderFieldBuild for PersonBuilderAgeAdded {
        type Built = u32;
        fn build(self) -> Self::Built {
            self.0
        }
    }
    pub(super) struct PersonBuilderEmailEmpty;
    pub(super) struct PersonBuilderEmailAdded(Option<String>);
    impl PersonBuilderFieldBuild for PersonBuilderEmailAdded {
        type Built = Option<String>;
        fn build(self) -> Self::Built {
            self.0
        }
    }
    impl Person {
        pub(super) fn builder(
        ) -> PersonBuilder<PersonBuilderNameEmpty, PersonBuilderAgeEmpty, PersonBuilderEmailEmpty>
        {
            PersonBuilder {
                name: PersonBuilderNameEmpty,
                age: PersonBuilderAgeEmpty,
                email: PersonBuilderEmailEmpty,
            }
        }
    }
    impl<AgeGenericParam, EmailGenericParam>
        PersonBuilder<PersonBuilderNameEmpty, AgeGenericParam, EmailGenericParam>
    {
        pub(super) fn name(
            self,
            name: String,
        ) -> PersonBuilder<PersonBuilderNameAdded, AgeGenericParam, EmailGenericParam> {
            PersonBuilder {
                name: PersonBuilderNameAdded(name),
                age: self.age,
                email: self.email,
            }
        }
    }
    impl<NameGenericParam, EmailGenericParam>
        PersonBuilder<NameGenericParam, PersonBuilderAgeEmpty, EmailGenericParam>
    where
        NameGenericParam: PersonBuilderFieldBuild,
    {
        pub(super) fn age(
            self,
            age: u32,
        ) -> PersonBuilder<NameGenericParam, PersonBuilderAgeAdded, EmailGenericParam> {
            PersonBuilder {
                name: self.name,
                age: PersonBuilderAgeAdded(age),
                email: self.email,
            }
        }
    }
    impl<NameGenericParam, AgeGenericParam>
        PersonBuilder<NameGenericParam, AgeGenericParam, PersonBuilderEmailEmpty>
    where
        NameGenericParam: PersonBuilderFieldBuild,
    {
        pub(super) fn email(
            self,
            email: Option<String>,
        ) -> PersonBuilder<NameGenericParam, AgeGenericParam, PersonBuilderEmailAdded> {
            PersonBuilder {
                name: self.name,
                age: self.age,
                email: PersonBuilderEmailAdded(email),
            }
        }
    }
    impl<NameGenericParam>
        PersonBuilder<NameGenericParam, PersonBuilderAgeAdded, PersonBuilderEmailAdded>
    where
        NameGenericParam: PersonBuilderFieldBuild<Built = String>,
    {
        pub(super) fn build(self) -> Person {
            Person {
                name: self.name.build(),
                age: self.age.build(),
                email: self.email.build(),
            }
        }
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
