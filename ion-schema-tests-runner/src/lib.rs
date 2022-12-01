extern crate proc_macro;

use darling::FromMeta;
use proc_macro::TokenStream as _TokenStream;
use regex::{Regex, RegexSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use syn::parse_macro_input;
use syn::{AttributeArgs, Lit, NestedMeta};

mod gen;
mod test_case_model;

/// Thin wrapper around PathBuf so that we can implement the FromMeta trait and get better
/// compile-time error messages if the path is not valid.
#[derive(Debug)]
struct MyPathBuf(PathBuf);
impl FromMeta for MyPathBuf {
    fn from_string(value: &str) -> darling::Result<Self> {
        Path::new(value)
            .canonicalize()
            .map(MyPathBuf)
            .map_err(darling::Error::custom)
    }
}
impl From<MyPathBuf> for PathBuf {
    fn from(value: MyPathBuf) -> Self {
        value.0
    }
}

/// Wrapper around a vec of strings so that we can implement the FromMeta trait and get better
/// compile-time error messages if one of the strings is not a valid regex string.
#[derive(Debug, Clone, Default)]
struct FilterStrings(Vec<String>);
impl FromMeta for FilterStrings {
    fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
        let mut the_vec: Vec<String> = vec![];
        let mut errors: Vec<darling::Error> = vec![];
        for item in items {
            match item {
                NestedMeta::Lit(Lit::Str(s)) => {
                    if let Err(e) = Regex::new(&s.value()) {
                        errors.push(darling::Error::custom(e).with_span(s));
                    } else {
                        the_vec.push(s.value());
                    }
                }
                _ => {
                    errors.push(darling::Error::unexpected_type("not a string").with_span(item));
                }
            }
        }
        if !errors.is_empty() {
            return Err(darling::Error::multiple(errors));
        }
        Ok(FilterStrings(the_vec))
    }
}

#[derive(Debug, FromMeta)]
struct MacroArgs {
    root: MyPathBuf,
    ignored: Option<FilterStrings>,
}

#[proc_macro]
pub fn ion_schema_tests(item: _TokenStream) -> _TokenStream {
    let attr_args = parse_macro_input!(item as AttributeArgs);

    let args: MacroArgs = match MacroArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return e.write_errors().into();
        }
    };

    let root_dir = PathBuf::from(args.root);

    let re = match args.ignored {
        None => RegexSet::empty(),
        Some(strings) => RegexSet::new(strings.0).unwrap(),
    };
    let exclude_filter = Rc::new(move |s: String| re.is_match(&s));

    gen::generate_test_root_module(root_dir.as_path(), exclude_filter).into()
}
