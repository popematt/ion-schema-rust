// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::ion_extension::ElementExtensions;
use crate::loader::schema_reader::SchemaReaderState::BeforeHeader;
use crate::loader::{DocumentContent, ReadFromIsl, ReadResultWithPartialSuccess, ReaderContext};
use crate::model::{SchemaDocument, SchemaFooter, SchemaHeader, SchemaItem, TypeDefinition};
use crate::result::{invalid_schema_2, InvalidSchemaError, InvalidSchemaErrorCollector};
use crate::{IslVersion, ISL_1_0, ISL_2_0, ISL_VERSION_MARKER_REGEX, RESERVED_WORD_REGEX};
use ion_rs::{Element, Sequence};
use std::mem::{replace, take};

macro_rules! impl_try_from {
    ($T:ty) => {
        impl TryFrom<$T> for SchemaDocument {
            type Error = InvalidSchemaError;

            fn try_from(value: $T) -> Result<Self, Self::Error> {
                let mut reader = SchemaReader::new();
                value.into_iter().for_each(|el| reader.consume(el));
                reader.close().map_err(|(_bad_schema, error)| error)
            }
        }
    };
}
impl_try_from!(Sequence);
impl_try_from!(Vec<Element>);
impl_try_from!(DocumentContent);

pub(super) fn read_isl(value: DocumentContent) -> ReadResultWithPartialSuccess<SchemaDocument> {
    let mut reader = SchemaReader::new();
    value.into_iter().for_each(|el| reader.consume(el));
    reader.close()
}

/// A helper type for encapsulating the state needed to read a [`SchemaDocument`].
struct SchemaReader {
    pending_elements: Vec<Element>,
    state: SchemaReaderState,
    schema_items: Vec<SchemaItem>,
    reader_context: Context,
    error_collector: InvalidSchemaErrorCollector,
}

#[derive(Debug)]
enum UnknownVersion {}

#[derive(Debug)]
enum Context {
    UnknownVersion(ReaderContext<UnknownVersion>),
    ISL_1_0(ReaderContext<ISL_1_0>),
    ISL_2_0(ReaderContext<ISL_2_0>),
}
impl Context {
    fn major_minor(&self) -> (u8, u8) {
        match self {
            Context::UnknownVersion(_) => (0, 0),
            Context::ISL_1_0(_) => (1, 0),
            Context::ISL_2_0(_) => (2, 0),
        }
    }

    fn set_version<V: IslVersion>(&mut self, f: impl FnOnce(ReaderContext<V>) -> Context) {
        match self {
            Context::UnknownVersion(c) => {
                let replacement = f(c.with_version());
                let _ = replace(self, replacement);
            }
            _ => unreachable!(),
        }
    }
}
macro_rules! with_context {
    ($ctx:expr, $action:expr) => {
        match $ctx {
            Context::UnknownVersion(_) => unreachable!(),
            Context::ISL_1_0(ctx) => $action(ctx),
            Context::ISL_2_0(ctx) => $action(ctx),
        }
    };
}

#[derive(Debug, Copy, Clone)]
enum SchemaReaderState {
    BeforeHeader,
    ReadingTypes,
    AfterFooter,
}
#[derive(Debug, Copy, Clone)]
enum IslSyntaxKind {
    OpenContent,
    Header,
    Footer,
    Type,
}

impl SchemaReader {
    pub(crate) fn new() -> Self {
        let context = ReaderContext::<UnknownVersion>::new();
        Self {
            pending_elements: vec![],
            schema_items: vec![],
            state: BeforeHeader,
            reader_context: Context::UnknownVersion(ReaderContext::new()),
            error_collector: InvalidSchemaErrorCollector::default(),
        }
    }

    pub fn consume(&mut self, element: Element) {
        let result = match self.reader_context {
            Context::UnknownVersion(_) => self.consume_unknown_version(element),
            _ => self.private_consume(element),
        };
        self.error_collector.ok_or_push_err(result);
    }

    fn consume_unknown_version(&mut self, element: Element) -> Result<(), InvalidSchemaError> {
        let ann = element.annotations();
        if ann.is_empty() {
            // Check for a version marker
            if let Some(symbol) = element.as_symbol() {
                let maybe_isl_version = element.require_known_symbol("top-level symbol")?;
                if ISL_VERSION_MARKER_REGEX.is_match(maybe_isl_version) {
                    return match maybe_isl_version {
                        ISL_1_0::VERSION_MARKER_TEXT => {
                            self.reader_context.set_version(Context::ISL_1_0);
                            self.consume_pending_open_content();
                            Ok(())
                        }
                        ISL_2_0::VERSION_MARKER_TEXT => {
                            self.reader_context.set_version(Context::ISL_2_0);
                            self.consume_pending_open_content();
                            Ok(())
                        }
                        other => invalid_schema_2!(
                            &element,
                            "unknown/unsupported Ion Schema Version: {other}"
                        ),
                    };
                }
            }
        } else if ann.contains("type")
            || ann.contains("schema_header")
            || ann.contains("schema_footer")
        {
            // If we see a header, footer, or type before a version marker, then it's ISL 1.0
            self.reader_context.set_version(Context::ISL_1_0);
            self.consume_pending_open_content();
            return self.private_consume(element);
        }
        self.pending_elements.push(element);
        Ok(())
    }

    fn consume_pending_open_content(&mut self) {
        let pending_open_content = take(&mut self.pending_elements);
        pending_open_content.into_iter().for_each(|element| {
            let result = self.private_consume(element);
            self.error_collector.ok_or_push_err(result);
        });
    }

    fn private_consume(&mut self, element: Element) -> Result<(), InvalidSchemaError> {
        let ann = element.annotations();
        use IslSyntaxKind::*;
        let syntax_kind = if ann.contains("type") {
            Type
        } else if ann.contains("schema_header") {
            Header
        } else if ann.contains("schema_footer") {
            Footer
        } else {
            OpenContent
        };

        use SchemaReaderState::*;
        self.state = match (syntax_kind, self.state) {
            (OpenContent, state) => {
                if self.reader_context.major_minor() != (1, 0) && !ann.is_empty() {
                    let first_annotation =
                        ann.iter().next().unwrap().expect_text().map_err(|_| {
                            invalid_schema_2!(
                                &element,
                                "found top-level value annotated with unknown symbol text"
                            )
                        })?;
                    if RESERVED_WORD_REGEX.is_match(first_annotation) {
                        invalid_schema_2!(
                            &element,
                            "extra content cannot be annotated with reserved word: {element}"
                        )?;
                    }
                }
                if self.reader_context.major_minor() != (1, 0) && ann.is_empty() {
                    if let Some(maybe_isl_version) = element.as_symbol().and_then(|e| e.text()) {
                        if ISL_VERSION_MARKER_REGEX.is_match(maybe_isl_version) {
                            invalid_schema_2!(
                                &element,
                                "unexpected Ion Schema version marker: {element}"
                            )?;
                        }
                    }
                }
                self.schema_items.push(SchemaItem::OpenContent(element));
                state
            }
            (Header, BeforeHeader) => {
                let header = with_context!(&self.reader_context, |ctx| {
                    SchemaHeader::try_read(&element, ctx)
                });
                self.schema_items.push(SchemaItem::Header(header?));
                ReadingTypes
            }
            (Type, ReadingTypes | BeforeHeader) => {
                let name = element
                    .get_required_field("type definition", "name")?
                    .require_known_symbol("type name")?;
                let type_def = with_context!(&self.reader_context, |ctx| {
                    TypeDefinition::try_read(&element, ctx)
                });
                self.schema_items
                    .push(SchemaItem::Type(name.into(), type_def?));
                ReadingTypes
            }
            (Footer, ReadingTypes | BeforeHeader) => {
                let footer = with_context!(&self.reader_context, |ctx| {
                    SchemaFooter::try_read(&element, ctx)
                });
                self.schema_items.push(SchemaItem::Footer(footer?));
                AfterFooter
            }
            (syntax, state) => {
                invalid_schema_2!(&element, "illegal {syntax:?} while {state:?}: {element}")?
            }
        };
        Ok(())
    }

    pub fn close(mut self) -> ReadResultWithPartialSuccess<SchemaDocument> {
        if matches!(self.reader_context, Context::UnknownVersion(_)) {
            // No ISL has been discovered yet. Weird, but not invalid.
            self.reader_context.set_version(Context::ISL_1_0);
            self.consume_pending_open_content();
        }

        let schema = SchemaDocument::new(self.reader_context.major_minor(), self.schema_items);

        self.error_collector.into_partial_result_with(schema)
    }
}

#[cfg(test)]
mod tests {
    use crate::loader::schema_reader::SchemaReader;
    use crate::loader::ReadResult;
    use crate::model::SchemaDocument;
    use ion_rs::Element;
    // With/without version marker
    // With/without header
    // With/without types
    // With/without open content
    // With/without footer

    // Things in wrong order

    #[test]
    fn read_isl_1_0() {
        let schema_document = Element::read_all(
            r"
        $ion_schema_1_0
        type::{
          name: foo,
        }
        ",
        )
        .unwrap();
        let mut reader = SchemaReader::new();
        schema_document
            .into_iter()
            .for_each(|el| reader.consume(el));
        let schema = reader.close().unwrap();
    }

    macro_rules! assert_reader_err {
        ($result:expr $(, $fun:expr)?) => {
            match $result {
                Ok(_) => panic!("expected a schema reader error"),
                Err(e) => {
                    println!("{e}");
                    $($fun(e);)?
                }
            }
        };
    }

    #[test]
    fn read_lots_of_errors() {
        let schema_ion = Element::read_all(r#"
        $ion_schema_2_0
        type::{ named: foo }
        type::{ name: baz, type: { id: "foo.isl", type: quux } }
        type::{ name: bar, valid_values: [foo::a, range::["a", "z"], range::[exclusive::min, +inf]], type: "symbol" }
        $ion_schema_2_0
        "#).unwrap();
        let schema: ReadResult<SchemaDocument> = schema_ion.try_into();
        assert_reader_err!(schema)
    }
}
