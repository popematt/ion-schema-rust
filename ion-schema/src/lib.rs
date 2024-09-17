// TODO: remove the following line once we have a basic implementation ready
#![allow(dead_code, unused_variables)]

use crate::ion_path::IonPath;
use crate::isl::isl_constraint::IslConstraintValue;
use crate::isl::isl_type::IslType;
use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use crate::violation::{Violation, ViolationCode};
use ion_rs::{Element, Struct, Symbol, Sequence, IonType, WriteAsIon, ValueWriter, IonResult, StructWriter};
use regex::Regex;
use std::fmt::{Display, Formatter};
use std::sync::OnceLock;

/// A `try`-like macro to work around the [`Option`]/[`Result`] nested APIs.
/// These API require checking the type and then calling the appropriate getter function
/// (which returns a None if you got it wrong). This macro turns the `None` into
/// an `IonSchemaError` which cannot be currently done with `?`.
macro_rules! try_to {
    ($getter:expr) => {
        match $getter {
            Some(value) => value,
            None => invalid_schema_error(format!("Missing a value: {}", stringify!($getter)))?,
        }
    };
}

// TODO: consider changing some of these modules to public if required
pub mod authority;
mod constraint;
mod import;
pub(crate) mod ion_extension;
mod ion_path;
pub mod isl;
mod nfa;
pub mod result;
pub mod schema;
pub mod system;
mod type_reference;
pub mod types;
pub mod violation;

/// Re-export of the ion-rs dependency that is part of our public API.
pub mod external {
    pub use ion_rs;
}

static ISL_VERSION_MARKER_REGEX: OnceLock<Regex> = OnceLock::new();
static RESERVED_WORD_REGEX: OnceLock<Regex> = OnceLock::new();

/// Checks if a value is an ISL version marker.
fn is_isl_version_marker(text: &str) -> bool {
    ISL_VERSION_MARKER_REGEX
        .get_or_init(|| Regex::new(r"^\$ion_schema_\d.*$").unwrap())
        .is_match(text)
}

/// Checks is a value is reserved keyword ISL version maker.
fn is_reserved_word(text: &str) -> bool {
    RESERVED_WORD_REGEX
        .get_or_init(|| Regex::new(r"^(\$ion_schema(_.*)?|[a-z][a-z0-9]*(_[a-z0-9]+)*)$").unwrap())
        .is_match(text)
}

const ISL_2_0_KEYWORDS: [&str; 28] = [
    "all_of",
    "annotations",
    "any_of",
    "as",
    "byte_length",
    "codepoint_length",
    "container_length",
    "contains",
    "element",
    "exponent",
    "field_names",
    "fields",
    "id",
    "imports",
    "name",
    "not",
    "occurs",
    "one_of",
    "ordered_elements",
    "precision",
    "regex",
    "schema_footer",
    "schema_header",
    "timestamp_precision",
    "type",
    "user_reserved_fields",
    "utf8_byte_length",
    "valid_values",
];

#[derive(Debug, Clone, PartialEq)]
enum IonSchemaElementKind<'a> {
    SingleElement(&'a Element),
    Document(&'a Sequence),
    // Unfortunately, sometimes we have to convert to an owned sequence.
    // This is because SequenceIterator is not exposed, so we can't create
    // an enum that unifies `&'a Sequence` and `&'a [Element]`.
    OwnedDocument(Sequence),
}

// Ugh... this type is public.
pub enum Document<'a> {
    Borrowed(&'a Sequence),
    Owned(Sequence),
}

pub trait AsDocument<'a> {
  fn as_document(&'a self) -> Document<'a>;
}

impl <'a> AsDocument<'a> for Sequence {
    fn as_document(&'a self) -> Document<'a> {
        Document::Borrowed(self)
    }
}

pub trait ToDocument<'a> {
    fn to_document(self) -> Document<'a>;
}

impl <'a> ToDocument<'a> for Vec<Element> {
    fn to_document(self) -> Document<'a> {
        Document::Owned(Sequence::from(self))
    }
}

impl <'a> From<&'a Element> for IonSchemaElement<'a> {
    fn from(value: &'a Element) -> Self {
        if value.annotations().contains("document") {
            todo!();
        }
        IonSchemaElement { content: IonSchemaElementKind::SingleElement(value) }
    }
}

impl <'a> From<Document<'a>> for IonSchemaElement<'a> {
    fn from(value: Document<'a>) -> Self {
        let content = match value {
            Document::Borrowed(s) => IonSchemaElementKind::Document(s),
            Document::Owned(s) => IonSchemaElementKind::OwnedDocument(s),
        };
        IonSchemaElement { content }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum IonSchemaElementType {
    Null,
    Bool,
    Int,
    Float,
    Decimal,
    Timestamp,
    Symbol,
    String,
    Clob,
    Blob,
    List,
    SExp,
    Struct,
    Document,
}

impl Display for IonSchemaElementType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            IonSchemaElementType::Null => "null",
            IonSchemaElementType::Bool => "bool",
            IonSchemaElementType::Int => "int",
            IonSchemaElementType::Float => "float",
            IonSchemaElementType::Decimal => "decimal",
            IonSchemaElementType::Timestamp => "timestamp",
            IonSchemaElementType::Symbol => "symbol",
            IonSchemaElementType::String => "string",
            IonSchemaElementType::Clob => "clob",
            IonSchemaElementType::Blob => "blob",
            IonSchemaElementType::List => "list",
            IonSchemaElementType::SExp => "sexp",
            IonSchemaElementType::Struct => "struct",
            IonSchemaElementType::Document => "document",
        };
        f.write_str(text)
    }
}

impl From<IonType> for IonSchemaElementType {
    fn from(value: IonType) -> Self {
        match value {
            IonType::Null => IonSchemaElementType::Null,
            IonType::Bool => IonSchemaElementType::Bool,
            IonType::Int => IonSchemaElementType::Int,
            IonType::Float => IonSchemaElementType::Float,
            IonType::Decimal => IonSchemaElementType::Decimal,
            IonType::Timestamp => IonSchemaElementType::Timestamp,
            IonType::Symbol => IonSchemaElementType::Symbol,
            IonType::String => IonSchemaElementType::String,
            IonType::Clob => IonSchemaElementType::Clob,
            IonType::Blob => IonSchemaElementType::Blob,
            IonType::List => IonSchemaElementType::List,
            IonType::SExp => IonSchemaElementType::SExp,
            IonType::Struct => IonSchemaElementType::Struct,
        }
    }
}

/// Provide an Ion schema Element which includes all Elements and a document type
///
/// An [IonSchemaElement] can be constructed from [&Element] or [Element] to represent a single
/// Ion value, or from [Sequence] and [&Sequence] to represent a document.
///
/// ## Example:
/// In general `TypeRef` `validate()` takes in IonSchemaElement as the value to be validated.
/// In order to create an `IonSchemaElement`:
///
/// ```
/// use ion_rs::Element;
/// use ion_schema::IonSchemaElement;
///
/// // create an IonSchemaElement from an Element by borrowing it
/// let element0: Element = 0.into();
/// let ion_schema_element: IonSchemaElement = (&element0).into();
///
/// // create an IonSchemaElement from an Element by taking ownership of it
/// let element1: Element = 1.into();
/// let ion_schema_element: IonSchemaElement = element1.into();
///
/// // create an IonSchemaElement for document type based on vector of elements
/// let elements1: Vec<Element> = vec![2.into()];
/// let document: IonSchemaElement = elements1.into();
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct IonSchemaElement<'a> {
    content: IonSchemaElementKind<'a>
}

impl <'a> IonSchemaElement<'a> where Self: 'a {
    pub fn as_sequence(&'a self) -> Option<&'a Sequence> {
        match &self.content {
            IonSchemaElementKind::SingleElement(e) => e.as_sequence(),
            IonSchemaElementKind::Document(seq) => Some(seq),
            IonSchemaElementKind::OwnedDocument(seq) => Some(seq),
        }
    }

    pub fn as_struct(&'a self) -> Option<&'a Struct> {
        match self.content {
            IonSchemaElementKind::SingleElement(e) => e.as_struct(),
            _ => None
        }
    }

    pub fn as_element(&'a self) -> Option<&'a Element> {
        match self.content {
            IonSchemaElementKind::SingleElement(element) => Some(element),
            _ => None,
        }
    }

    pub fn as_document(&'a self) -> Option<&'a Sequence> {
        match self.content {
            IonSchemaElementKind::Document(seq) => Some(seq),
            _ => None,
        }
    }

    pub fn ion_schema_type(&self) -> IonSchemaElementType {
        match self.as_element() {
            Some(e) => e.ion_type().into(),
            _ => IonSchemaElementType::Document,
        }
    }

    pub fn is_null(&self) -> bool {
        match self.as_element() {
            Some(e) => e.is_null(),
            _ => false
        }
    }

    fn expect_element_of_type(
        &self,
        types: &[IonType],
        constraint_name: &str,
        ion_path: &mut IonPath,
    ) -> Result<&Element, Violation> {
        match self.as_element() {
            Some(element) => {
                if !types.contains(&element.ion_type()) || element.is_null() {
                    // If it's an Element but the type isn't one of `types`,
                    // return a Violation with the constraint name.
                    return Err(Violation::new(
                        constraint_name,
                        ViolationCode::TypeMismatched,
                        format!("expected {:?} but found {}", types, element.ion_type()),
                        ion_path,
                    ));
                }
                // If it's an Element of an expected type, return a ref to it.
                Ok(element)
            }
            None => {
                // If it's a Document, return a Violation with the constraint name
                Err(Violation::new(
                    constraint_name,
                    ViolationCode::TypeMismatched,
                    format!("expected {types:?} but found document"),
                    ion_path,
                ))
            }
        }
    }
}

impl <'a> Display for IonSchemaElement<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.content {
            IonSchemaElementKind::SingleElement(element) => {
                write!(f, "{element}")
            }
            IonSchemaElementKind::Document(document) => {
                write!(f, "/* Ion document */ ")?;
                for value in document.iter() {
                    write!(f, "{value} ")?;
                }
                write!(f, "/* end */")
            }
            IonSchemaElementKind::OwnedDocument(document) => {
                write!(f, "/* Ion document */ ")?;
                for value in document.iter() {
                    write!(f, "{value} ")?;
                }
                write!(f, "/* end */")
            }
        }
    }
}

// helper function to be used by schema tests
fn load(text: &str) -> Vec<Element> {
    Element::read_all(text.as_bytes()).expect("parsing failed unexpectedly").into_iter().collect()
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct UserReservedFields {
    schema_header_fields: Vec<String>,
    schema_footer_fields: Vec<String>,
    type_fields: Vec<String>,
}

impl UserReservedFields {

    pub(crate) fn is_empty(&self) -> bool {
        self.type_fields.is_empty() && self.schema_header_fields.is_empty() && self.schema_footer_fields.is_empty()
    }

    /// Parse use reserved fields inside a [Struct]
    pub(crate) fn from_ion_elements(user_reserved_fields: &Struct) -> IonSchemaResult<Self> {
        if user_reserved_fields.fields().any(|(f, v)| {
            f.text() != Some("schema_header")
                && f.text() != Some("schema_footer")
                && f.text() != Some("type")
        }) {
            return invalid_schema_error(
                "User reserved fields can only have schema_header, schema_footer or type as the field names",
            );
        }
        Ok(Self {
            schema_header_fields: UserReservedFields::field_names_from_ion_elements(
                "schema_header",
                user_reserved_fields,
            )?,
            schema_footer_fields: UserReservedFields::field_names_from_ion_elements(
                "schema_footer",
                user_reserved_fields,
            )?,
            type_fields: UserReservedFields::field_names_from_ion_elements(
                "type",
                user_reserved_fields,
            )?,
        })
    }

    fn field_names_from_ion_elements(
        user_reserved_fields_type: &str,
        user_reserved_fields: &Struct,
    ) -> IonSchemaResult<Vec<String>> {
        let user_reserved_elements: Vec<&Element> = user_reserved_fields
            .get(user_reserved_fields_type)
            .and_then(|it| it.as_sequence().map(|s| s.elements().collect()))
            .ok_or(invalid_schema_error_raw(
                "User reserved fields mut be non null",
            ))?;

        let user_reserved_fields = user_reserved_elements
            .iter()
            .filter(|e| e.annotations().is_empty() && !e.is_null())
            .map(|e| e.as_text().map(|s| s.to_owned()))
            .collect::<Option<Vec<String>>>()
            .unwrap_or(vec![]);

        if user_reserved_fields.len() != user_reserved_elements.len() {
            return invalid_schema_error("User reserved fields mut be unannotated");
        }

        if user_reserved_fields
            .iter()
            .any(|f| is_reserved_word(f) || ISL_2_0_KEYWORDS.binary_search(&f.as_str()).is_ok())
        {
            return invalid_schema_error(
                "ISl 2.0 keywords may not be declared as user reserved fields",
            );
        }

        Ok(user_reserved_fields)
    }

    pub(crate) fn validate_field_names_in_header(
        &self,
        schema_header: &Struct,
    ) -> IonSchemaResult<()> {
        let unexpected_fields: Vec<(&Symbol, &Element)> = schema_header
            .fields()
            .filter(|(f, v)| {
                !self
                    .schema_header_fields
                    .contains(&f.text().unwrap().to_owned())
                    && f.text().unwrap() != "user_reserved_fields"
                    && f.text().unwrap() != "imports"
            })
            .collect();

        if !unexpected_fields.is_empty() {
            // for unexpected fields return invalid schema error
            return invalid_schema_error(format!(
                "schema header contains unexpected fields: {unexpected_fields:?}"
            ));
        }

        Ok(())
    }

    pub(crate) fn validate_field_names_in_type(&self, isl_type: &IslType) -> IonSchemaResult<()> {
        let unexpected_fields: &Vec<&String> = &isl_type
            .constraints()
            .iter()
            .filter(|c| matches!(c.constraint_value, IslConstraintValue::Unknown(_, _)))
            .map(|c| match &c.constraint_value {
                IslConstraintValue::Unknown(f, v) => f,
                _ => {
                    unreachable!("we have already filtered all other constraints")
                }
            })
            .filter(|f| !self.type_fields.contains(f))
            .collect();

        if !unexpected_fields.is_empty() {
            // for unexpected fields return invalid schema error
            return invalid_schema_error(format!(
                "schema type contains unexpected fields: {unexpected_fields:?}"
            ));
        }

        Ok(())
    }

    pub(crate) fn validate_field_names_in_footer(
        &self,
        schema_footer: &Struct,
    ) -> IonSchemaResult<()> {
        let unexpected_fields: Vec<(&Symbol, &Element)> = schema_footer
            .fields()
            .filter(|(f, v)| {
                !self
                    .schema_footer_fields
                    .contains(&f.text().unwrap().to_owned())
            })
            .collect();

        if !unexpected_fields.is_empty() {
            // for unexpected fields return invalid schema error
            return invalid_schema_error(format!(
                "schema footer contains unexpected fields: {unexpected_fields:?}"
            ));
        }
        Ok(())
    }
}

impl WriteAsIon for UserReservedFields {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        let mut struct_writer = writer.struct_writer()?;
        struct_writer.field_writer("schema_header").write(&self.schema_header_fields)?;
        struct_writer.field_writer("type").write(&self.type_fields)?;
        struct_writer.field_writer("schema_footer").write(&self.schema_footer_fields)?;
        struct_writer.close()
    }
}
