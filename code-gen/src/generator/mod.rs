use std::fs;
use crate::generator::kotlin::{KotlinFile, KotlinGenerator, KotlinGeneratorOptions};
use crate::generator::rust::{pretty_print, RustGenerator, RustGeneratorOptions};
use crate::model::Universe;

mod rust;
mod kotlin;

pub trait Generator<Opts> {
    fn generate(&self, opts: &Opts) -> crate::Result<()>;
}

impl Generator<RustGeneratorOptions> for Universe {
    fn generate(&self, opts: &RustGeneratorOptions) -> crate::Result<()> {
        let mut generator = RustGenerator::new(self, opts);
        let ts = generator.generate_universe();

        let file = opts.output_dir.join(format!("{}.rs", &self.name));
        println!("{}", file.to_str().unwrap());
        fs::create_dir_all(&opts.output_dir)?;
        fs::write(file, pretty_print(&ts))?;
        Ok(())
    }
}

impl Generator<KotlinGeneratorOptions> for Universe {
    fn generate(&self, opts: &KotlinGeneratorOptions) -> crate::Result<()> {
        let generator = KotlinGenerator::new(self, opts);
        let files = generator.generate_universe();

        for f in files {
            let KotlinFile(path, content) = f;
            let path = opts.output_dir.join(path);
            println!("{:?}", path);
            if let Some(dir) = path.parent() {
                fs::create_dir_all(dir)?;
            }
            fs::write(opts.output_dir.join(path), content)?;
        }
        Ok(())
    }
}

pub(crate) trait JoinToString<Item> {
    fn join_to_string<F>(&mut self, separator: &str, transform_fn: F) -> String
        where F: FnMut(Item) -> String;
}
impl <T: Iterator> JoinToString<T::Item> for T {
    fn join_to_string<F: FnMut(T::Item) -> String>(&mut self, separator: &str, transform_fn: F) -> String {
        self.map(transform_fn).reduce(|a, b| format!("{a}{separator}{b}")).unwrap_or_default()
    }
}




#[cfg(test)]
mod generator_tests {
    use crate::id;
    use std::rc::Rc;
    use std::collections::HashMap;
    use std::path::PathBuf;
    use ion_rs::IonType;
    use crate::generator::kotlin::KotlinVersion;
    use crate::model::*;
    use crate::model::TypeReference::*;


    #[test]
    fn test_kotlin() -> crate::Result<()> {
        use crate::generator::*;

        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("generated/kotlin");
        fs::create_dir_all(path.clone())?;

        let opts = KotlinGeneratorOptions {
            output_dir: path,
            root_package: "com.amazon.fooservice".to_string(),
            public_types: false,
            mutable_types: false,
            use_inline_wrappers_for_sealed_types: true,
            nest_variants_inside_sealed_types: false,
            kotlin_version: KotlinVersion::v1_6,
            map_type: "kotlin.collections.Map".to_string(),
        };

        let universe = sample_universe();

        universe.generate(&opts)
    }


    #[test]
    fn test_rust() -> crate::Result<()> {
        use crate::generator::*;
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("src");
        fs::create_dir_all(path.clone())?;

        let opts = RustGeneratorOptions {
            output_dir: path,
            public: false,
            generate_builders: true,
            map_type: "std::collections::HashMap".to_string(),
        };

        let universe = sample_universe();

        universe.generate(&opts)
    }


    pub(crate) fn sample_universe() -> Universe {
        Universe {
            name: "my_domain_model".to_string(),
            content: vec![
                ContainerBuilder::default()
                    .id(id!("unit"))
                    .self_type(TypeDefinition::Native {
                        qualified_names: HashMap::from([
                            ("rust".to_string(), "()".to_string()),
                            ("kotlin".to_string(), "kotlin.Unit".to_string()),
                        ]),
                    })
                    .build().unwrap(),
                ContainerBuilder::default()
                    .id(id!("float32"))
                    .self_type(TypeDefinition::Native {
                        qualified_names: HashMap::from([
                            ("rust".to_string(), "f32".to_string()),
                            ("kotlin".to_string(), "kotlin.Float".to_string()),
                        ]),
                    })
                    .build().unwrap(),
                ContainerBuilder::default()
                    .id(id!("float64"))
                    .self_type(TypeDefinition::Native {
                        qualified_names: HashMap::from([
                            ("rust".to_string(), "f64".to_string()),
                            ("kotlin".to_string(), "kotlin.Double".to_string()),
                        ]),
                    })
                    .build().unwrap(),
                ContainerBuilder::default()
                    .id(id!("my_cool_enum"))
                    .docs("\
                        This enum is meant to flange the widgets in the endoplasmic reticulum.
                    ")
                    .self_type(TypeDefinition::Enum {
                        values: vec![
                            "cold".to_string(),
                            "freezing".to_string(),
                            "chilly".to_string(),
                        ],
                    })
                    .build().unwrap(),
                ContainerBuilder::default()
                    .id(id!("AnyFloat"))
                    .self_type(TypeDefinition::Sum {
                        variants: HashMap::from([
                            ("float".to_string(), Rc::new(Local(id!("float32")))),
                            ("double".to_string(), Rc::new(Local(id!("float64")))),
                            ("ion_float".to_string(), Rc::new(IonValueType(IonType::Float))),
                        ]),
                    })
                    .build().unwrap(),
                ContainerBuilder::default()
                    .id(id!("person"))
                    .self_type(TypeDefinition::Record {
                        components: HashMap::from([
                            ("first_name".to_string(), Rc::new(IonSchemaType(IonType::String))),
                            ("last_name".to_string(), Rc::new(IonSchemaType(IonType::String))),
                            ("age".to_string(), Rc::new(IonSchemaType(IonType::Integer))),
                        ]),
                    })
                    .build().unwrap(),
            ]
        }
    }
}
