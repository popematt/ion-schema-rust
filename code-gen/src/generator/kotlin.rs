use std::path::{Path, PathBuf};
use convert_case::{Case, Casing};
use quote::{format_ident, quote};
use std::collections::HashMap;
use std::fmt::format;
use std::fs;
use std::ops::AddAssign;
use std::ptr::null;
use ion_rs::IonType;
use crate::generator::{Generator, JoinToString};
use crate::generator::kotlin::KotlinVersion::v1_5;
use crate::model::*;
use crate::model::TypeReference::*;

#[derive(Debug, Clone, Eq, PartialEq, Builder)]
#[builder(setter(into))]
pub struct KotlinGeneratorOptions {
    pub output_dir: PathBuf,
    pub root_package: String,
    pub public_types: bool,
    pub mutable_types: bool,
    pub use_inline_wrappers_for_sealed_types: bool,
    pub nest_variants_inside_sealed_types: bool,
    pub kotlin_version: KotlinVersion,
    pub map_type: String,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum KotlinVersion {
    v1_4,
    v1_5,
    v1_6,
    v1_7,
    v1_8,
}

#[derive(Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct KotlinFile(pub PathBuf, pub String);
impl KotlinFile {
    pub fn new(package: String, file_name: String, content: String) -> Self {
        let path_buf = PathBuf::from_iter(package.split('.')).join(file_name);
        let package_directive = if package.is_empty() { String::new() } else { format!("package {package};\n") };
        KotlinFile(path_buf, format!("{}{}", package_directive, KotlinFile::indent(content)))
    }

    fn indent(code: String) -> String {
        let mut indent = 0;
        let mut result: Vec<String> = Vec::new();
        for line in code.split("\n") {

            let line = line.trim();

            if line.starts_with(|c| ")}]".contains(c)) && indent > 0 {
                indent -= 4;
            }

            let newline = " ".repeat(indent) + line;

            if newline.ends_with(|c| "({[".contains(c)) {
                indent += 4;
            }
            result.push(newline);
        }

        result.join("\n")
    }
}

// A is parent of B
// When
// A.self is None, and B.self is some, A is a package and B is a file/class
// A.self is Some, and B.self is None, A is a file/class B is a package.
// A.self is None, and B.self is None, A and B are packages.
// A.self is Some, and B.self is Some, A is a class, and B is a nested class

pub struct KotlinGenerator<'a> {
    opts: KotlinGeneratorOptions,
    universe: &'a Universe,
    reference_cache: HashMap<Id, String>,
    files: Vec<KotlinFile>
}
impl <'a> KotlinGenerator<'a> {
    pub fn new(universe: &'a Universe, opts: &KotlinGeneratorOptions) -> Self {
        KotlinGenerator {
            opts: opts.clone(),
            universe: universe,
            reference_cache: HashMap::new(),
            files: Vec::new(),
        }
    }

    fn quote_id(id: &Id) -> String {
        let path = id.parts.iter()
            .take(id.parts.len() - 1)
            .fold(String::new(), |a, b| format!("{}.{}", a, b.to_case(Case::Snake)));
        format!("{}.{}", path, id.name().to_case(Case::Pascal))
    }

    fn resolve_id(&mut self, id: &Id) -> String {
        match self.reference_cache.get(id) {
            Some(ts) => ts.clone(),
            None => {
                let (_, type_def) = self.universe.iter().find(|(item_id, _)| id == item_id)
                    .ok_or(format!("No type definition for id={}", id)).unwrap();

                let ts = match type_def {
                    TypeDefinition::Native { qualified_names } => {
                        let qualified_name = qualified_names.get("kotlin")
                            .ok_or(format!("No qualified name for Native type; id={}", id)).unwrap();
                        qualified_name.parse().unwrap()
                    }
                    _ => KotlinGenerator::quote_id(id)
                };

                self.reference_cache.insert(id.clone(), ts);
                self.reference_cache.get(id).unwrap().clone()
            }
        }
    }

    fn generate_reference(&mut self, type_ref: &TypeReference) -> String {
        match type_ref {
            Maybe {inner_ref, optional, nullable } => {
                let inner = self.generate_reference(inner_ref);
                if *optional && *nullable {
                    format!("Optional<{}?>", inner)
                } else if *optional {
                    format!("Optional<{}>", inner)
                } else if *nullable {
                    format!("{}?", inner)
                } else {
                    panic!("Maybe ref is not optional or nullable: {:?}", type_ref)
                }
            }
            Collection { outer_ref, element_ref } => {
                let outer = self.resolve_id(outer_ref);
                let element = self.generate_reference(element_ref);
                format!("{outer}<{element}>")
            }
            Map { key_ref, value_ref } => {
                let key = self.generate_reference(key_ref);
                let value = self.generate_reference(value_ref);
                format!("Map<{key}, {value}>")
            }
            Local(id) => {
                self.resolve_id(id)
            }
            IonValueType(t) => {
                // Need to generate all as IonElement so that they can hold Ion nulls.
                "com.amazon.ionelement.IonElement".to_string()
            }
            IonSchemaType(t) => {
                match t {
                    IonType::Null => unreachable!("Ion Schema does not have a 'null' type. Only '$null'."),
                    IonType::Boolean => "com.amazon.ionelement.BooleanElement",
                    IonType::Integer => "com.amazon.ionelement.IntegerElement",
                    IonType::Float => "com.amazon.ionelement.FloatElement",
                    IonType::Decimal => "com.amazon.ionelement.DecimalElement",
                    IonType::Timestamp => "com.amazon.ionelement.TimestampElement",
                    IonType::Symbol => "com.amazon.ionelement.SymbolElement",
                    IonType::String => "com.amazon.ionelement.StringElement",
                    IonType::Clob => "com.amazon.ionelement.ClobElement",
                    IonType::Blob => "com.amazon.ionelement.BlobElement",
                    IonType::List => "com.amazon.ionelement.ListElement",
                    IonType::SExpression => "com.amazon.ionelement.SExpressionElement",
                    IonType::Struct => "com.amazon.ionelement.StructElement",
                }.to_string()
            }
        }

    }

    fn generate_type(&mut self, id: &Id, type_def: &TypeDefinition, nested: String) -> String {
        let visibility = if self.opts.public_types { "public" } else { "internal" };
        let name = id.name().to_case(Case::Pascal);
        match &type_def {
            TypeDefinition::Enum { values } => {
                let enum_consts = values.iter().map(|it| it.to_case(Case::ScreamingSnake))
                    .reduce(|a, b| format!("{a},\n{b}"))
                    .unwrap();
                if nested.is_empty() {
                    format!(r#"
                        {visibility} enum class {name} {{
                            {enum_consts}
                        }}
                    "#)
                } else {
                    format!(r#"
                        {visibility} enum class {name} {{
                            {enum_consts};
                            {nested}
                        }}
                    "#)
                }
            },
            TypeDefinition::Sum { variants } => {
                let interface_or_abstract_class = if self.opts.kotlin_version > v1_5 { "interface" } else { "class" };
                let maybe_invoke_constructor = if self.opts.kotlin_version > v1_5 { "" } else { "()" };
                let variant_class_type = if self.opts.use_inline_wrappers_for_sealed_types {
                    if self.opts.kotlin_version >= v1_5 { "value" } else { "inline" }
                } else {
                    "data"
                };
                let enum_variants = variants.iter()
                    .map(|(variant_name, t_ref)| {
                        let r = self.generate_reference(t_ref);
                        let variant_name = variant_name.to_case(Case::Pascal);
                        format!("{visibility} {variant_class_type} class {variant_name}(val value: {r}): {name}{maybe_invoke_constructor}")
                    })
                    .reduce(|a, b| format!("{a}\n{b}"))
                    .unwrap_or_default();
                if self.opts.nest_variants_inside_sealed_types {
                    format!(r#"
                        {visibility} sealed {interface_or_abstract_class} {name} {{
                            {enum_variants}
                            {nested}
                        }}
                    "#)
                } else if !nested.is_empty() {
                    format!(r#"
                        {visibility} sealed {interface_or_abstract_class} {name} {{
                            {nested}
                        }}
                        {enum_variants}
                    "#)
                } else {
                    format!(r#"
                        {visibility} sealed {interface_or_abstract_class} {name}
                        {enum_variants}
                    "#)
                }
            }
            TypeDefinition::Interface { .. } => todo!(),
            TypeDefinition::Record { components } => {
                let mut_ = if self.opts.mutable_types { "var" } else { "val" };

                let fields = components.iter()
                    .map(|(prop_name, t_ref)| format!("{mut_} {}: {},", prop_name.to_case(Case::Camel), self.generate_reference(&t_ref)))
                    .reduce(|a, b| format!("{a}\n{b}"))
                    .unwrap();

                if nested.is_empty() {
                    format!(r#"
                        {visibility} data class {name}(
                            {fields}
                        )
                    "#)
                } else {
                    format!(r#"
                        {visibility} data class {name}(
                            {fields}
                        ) {{
                            {nested}
                        }}
                    "#)
                }
            }
            TypeDefinition::Tuple { components } => {
                let mut_ = if self.opts.mutable_types { "var" } else { "val" };

                let fields = components.iter().enumerate()
                    .map(|(idx, t_ref)| format!("{mut_} component{idx}: {},", self.generate_reference(&t_ref)))
                    .reduce(|a, b| format!("{a}\n{b}"))
                    .unwrap();

                if nested.is_empty() {
                    format!(r#"
                        {visibility} data class {name}(
                            {fields}
                        )
                    "#)
                } else {
                    format!(r#"
                        {visibility} data class {name}(
                            {fields}
                        ) {{
                            {nested}
                        }}
                    "#)
                }
            },
            TypeDefinition::Native { .. } => {
                // Output nothing since we resolve this as type references
                "".to_string()
            }
        }
    }

    fn generate_package(&mut self, container: &Container) {
        assert!(container.self_type.is_none());
        for child in &container.children {
            if child.self_type.is_some() {
                self.generate_file(&child)
            } else {
                self.generate_package(&child)
            }
        }
    }


    fn generate_file(&mut self, container: &Container) {
        let content = self.generate_class(container);
        let relative_package = container.id.ancestors().iter().join_to_string(".", |s| s.to_case(Case::Snake));
        let package_root = &self.opts.root_package;
        let package = if package_root.is_empty() {
            relative_package
        } else {
            format!("{package_root}.{relative_package}")
        };
        let file_name = container.id.name().to_case(Case::Pascal) + ".kt";
        self.files.push(KotlinFile::new(package, file_name, content))
    }

    fn generate_class(&mut self, container: &Container) -> String {
        let self_type = container.self_type.as_ref()
            .ok_or("Class must have a type definition.")
            .unwrap();


        let docs = match &container.docs {
            Some(s) => {
                let content = s.trim().split('\n').join_to_string("\n", |line| format!("* {line}"));
                format!("/**\n{content}\n*/\n")
            },
            None => String::new(),
        };

        let nested = container.children.iter()
            .join_to_string("\n", |it| self.generate_class(it));
        let generated_type = self.generate_type(&container.id, self_type, nested);
        format!("{}\n{}", docs.trim(), generated_type.trim())
    }

    pub(crate) fn generate_universe(mut self) -> Vec<KotlinFile> {
        let mut content = String::new();

        for child in &self.universe.content {
            if child.self_type.is_some() {
                content += &*self.generate_class(&child);
            } else {
                self.generate_package(&child)
            }
        }

        if !content.is_empty() {
            let file_name = self.universe.name.to_case(Case::Pascal) + ".kt";
            self.files.push(KotlinFile::new(self.opts.root_package, file_name, content))
        }
        self.files
    }


}
