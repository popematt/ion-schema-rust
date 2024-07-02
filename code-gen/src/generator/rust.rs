use std::path::PathBuf;
use convert_case::{Case, Casing};
use proc_macro2::{TokenStream};
use quote::{format_ident, quote};
use std::collections::HashMap;
use std::fs;
use ion_rs::IonType;
use crate::generator::JoinToString;
use crate::model::*;
use crate::model::TypeReference::*;


#[derive(Debug, Clone, Eq, PartialEq, Builder)]
#[builder(setter(into))]
pub struct RustGeneratorOptions {
    pub output_dir: PathBuf,
    pub public: bool,
    pub generate_builders: bool,
    /// The type used for a map
    pub map_type: String,
    // Ideally, we let users provide some annotations/attributes that they would like to apply.
    // Don't know exactly how that would work. Could be a config file of some sort that could
    // be read by the CLI and turned into opts here.
}



pub struct RustGenerator<'a> {
    opts: RustGeneratorOptions,
    universe: &'a Universe,
    reference_cache: HashMap<Id, TokenStream>,
}
impl <'a> RustGenerator<'a> {
    pub fn new(universe: &'a Universe, opts: &RustGeneratorOptions) -> Self {
        RustGenerator {
            opts: opts.clone(),
            universe: universe,
            reference_cache: HashMap::new(),
        }
    }

    fn quote_id(id: &Id) -> TokenStream {
        let path = id.parts.iter()
            .take(id.parts.len() - 1)
            .map(|it| format_ident!("{}", it));
        let name = format_ident!("{}", id.name().to_case(Case::Pascal));
        quote!(#(#path::)*#name)
    }

    fn resolve_id(&mut self, id: &Id) -> TokenStream {
        match self.reference_cache.get(id) {
            Some(ts) => ts.clone(),
            None => {
                let (_, type_def) = self.universe.iter().find(|(item_id, _)| id == item_id)
                    .ok_or(format!("No type definition for id={}", id)).unwrap();

                let ts = match type_def {
                    TypeDefinition::Native { qualified_names } => {
                        let qualified_name = qualified_names.get("rust")
                            .ok_or(format!("No qualified name for Native type; id={}", id)).unwrap();
                        qualified_name.parse().unwrap()
                    }
                    _ => RustGenerator::quote_id(id)
                };

                self.reference_cache.insert(id.clone(), ts);
                self.reference_cache.get(id).unwrap().clone()
            }
        }
    }

    fn generate_reference(&mut self, type_ref: &TypeReference) -> TokenStream {
        match type_ref {
            Maybe {inner_ref, optional, nullable } => {
                if !optional && !nullable {
                    panic!("Maybe ref is not optional or nullable: {:?}", type_ref)
                }
                let inner = self.generate_reference(inner_ref);
                quote!(std::option::Option<#inner>)
            }
            Collection { outer_ref, element_ref } => {
                let outer = self.resolve_id(outer_ref);
                let element = self.generate_reference(element_ref);
                quote!(#outer<#element>)
            }
            Map { key_ref, value_ref } => {
                let key = self.generate_reference(key_ref);
                let value = self.generate_reference(value_ref);
                quote!(std::collections::HashMap<#key, #value>)
            }
            Local(id) => {
                self.resolve_id(id)
            }
            IonValueType(_) => {
                // Always generate as owned element so that the generated model allows access to the
                // annotations. If we did specific values, then it wouldn't work.
                //let owned_element = format_ident!("{}::{}::{}", "ion_rs", "owned", "Element");
                quote!(ion_rs::value::owned::Element)
            }
            IonSchemaType(t) => {
                match t {
                    IonType::Null => quote!(()),
                    IonType::Boolean => quote!(bool),
                    IonType::Integer => quote!(ion_rs::types::integer::Integer),
                    IonType::Float => quote!(f64),
                    IonType::Decimal => quote!(ion_rs::types::integer::Decimal),
                    IonType::Timestamp => quote!(ion_rs::types::integer::Timestamp),
                    IonType::Symbol => quote!(String),
                    IonType::String => quote!(String),
                    IonType::Clob => quote!(Vec<u8>),
                    IonType::Blob => quote!(Vec<u8>),
                    IonType::List => quote!(Vec),
                    IonType::SExpression => quote!(Vec),
                    IonType::Struct => unreachable!("Any structs should be Record types or maps."),
                }
            }
        }

    }

    fn generate_container(&mut self, container: &Container) -> TokenStream {
        let mut ts = TokenStream::new();

        let docs = match &container.docs {
            Some(s) => {
                s.trim().split('\n')
                    .join_to_string("\n", |line| format!("/// {line}"))
            },
            None => String::new(),
        };
        let docs: TokenStream = docs.parse().unwrap();
        ts.extend(docs);

        if let Some(type_def) = &container.self_type {
            ts.extend(self.generate_type(&container.id, type_def))
        }

        if !&container.children.is_empty() {
            let visibility = if self.opts.public { quote!(pub) } else { quote!(pub(crate)) };

            let name = format_ident!("{}", &container.id.name().to_case(Case::Snake));



            let children = container.children.iter()
                .map(|it| self.generate_container(it));

            ts.extend(quote! {
                #visibility mod #name {
                    #(#children)
                    *
                }
            });
        }
        ts
    }

    fn generate_type(&mut self, id: &Id, type_def: &TypeDefinition) -> TokenStream {
        let mut ts = TokenStream::new();
        let visibility = if self.opts.public { quote!(pub) } else { quote!(pub(crate)) };
        let name = format_ident!("{}", id.name().to_case(Case::Pascal));
        match &type_def {
            TypeDefinition::Enum { values } => {
                let enum_consts = values.iter().map(|it| format_ident!("{}", it.to_case(Case::Pascal)));
                ts.extend(quote! (
                    #[derive(Debug, Clone, PartialEq)]
                    #visibility enum #name {
                        #(#enum_consts),*
                    }
                ))
            },
            TypeDefinition::Sum { variants } => {
                let enum_variants = variants.iter()
                    .map(|(name, t_ref)| {
                        let r = self.generate_reference(t_ref);
                        let name = format_ident!("{}", name.to_case(Case::Pascal));
                        quote!(#name(#r))
                    });
                ts.extend(quote! (
                    #[derive(Debug, Clone, PartialEq)]
                    #visibility enum #name {
                        #(#enum_variants),*
                    }
                ))
            }
            TypeDefinition::Interface { .. } => {}
            TypeDefinition::Record { components } => {
                let mut derives =  quote!(Debug, Clone, PartialEq);
                let mut additional_attributes = TokenStream::default();
                if self.opts.generate_builders {
                    derives.extend(quote!(,Builder));
                    additional_attributes.extend(quote!(#[builder(setter(into))]));
                }
                let field_name = components.iter()
                    .map(|(name, _)| format_ident!("{}", name.to_case(Case::Snake)));
                let field_type = components.iter()
                    .map(|(_, t_ref)| self.generate_reference(t_ref));

                ts.extend(quote! {
                    #[derive(#derives)]
                    #additional_attributes
                    #visibility struct #name {
                        #(#field_name: #field_type),*
                    }
                });
            }
            TypeDefinition::Tuple { components } => {
                let component_type = components.iter()
                    .map(|t_ref| self.generate_reference(&t_ref));

                ts.extend(quote! {
                    #[derive(Debug, Clone, PartialEq)]
                    #visibility struct #name (
                        #(#component_type),*
                    )
                });
            }
            TypeDefinition::Native { .. } => {
                // Do nothing since we resolve this as type references
            }
        }
        ts
    }

    pub(crate) fn generate_universe(&mut self) -> TokenStream {
        let children = self.universe.content.iter()
            .map(|it| self.generate_container(it));
        quote! {
            #(#children) *
        }
    }
}

pub(crate) fn pretty_print(ts: &TokenStream) -> String {
    let file = syn::parse_file(&ts.to_string()).unwrap();
    prettyplease::unparse(&file)
}

