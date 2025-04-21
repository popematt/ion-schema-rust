// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::ion_extension::ElementExtensions;
use crate::loader::schema_reader::SchemaReaderState::BeforeHeader;
use crate::loader::{ReadFromIsl, ReadResult, ReaderContext};
use crate::model::{SchemaDocument, SchemaFooter, SchemaHeader, SchemaItem, TypeDefinition};
use crate::result::{invalid_schema_2, InvalidSchemaError, InvalidSchemaErrorCollector};
use crate::{IslVersion, ISL_VERSION_MARKER_REGEX, RESERVED_WORD_REGEX};
use ion_rs::Element;

/// A helper type for encapsulating the state needed to read a [`SchemaDocument`].
pub(super) struct SchemaReader<V> {
    state: SchemaReaderState,
    schema_items: Vec<SchemaItem>,
    reader_context: ReaderContext<V>,
    error_collector: InvalidSchemaErrorCollector,
}

enum UnknownVersion {}

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

// impl SchemaReader<UnknownVersion> {
//     pub(crate) fn new() -> Self {
//
//     }
// }

impl<V: IslVersion> SchemaReader<V>
where
    TypeDefinition: ReadFromIsl<V>,
    SchemaHeader: ReadFromIsl<V>,
    SchemaFooter: ReadFromIsl<V>,
{
    pub fn new() -> Self {
        Self {
            schema_items: vec![],
            state: BeforeHeader,
            reader_context: ReaderContext::new(),
            error_collector: InvalidSchemaErrorCollector::default(),
        }
    }

    pub fn consume(&mut self, element: Element) {
        let result = self.private_consume(element);
        self.error_collector.ok_or_push_err(result);
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
                if V::MAJOR_MINOR != (1, 0) && !ann.is_empty() {
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
                if V::MAJOR_MINOR != (1, 0) && ann.is_empty() {
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
                self.schema_items
                    .push(SchemaItem::Header(SchemaHeader::try_read(
                        &element,
                        &self.reader_context,
                    )?));
                ReadingTypes
            }
            (Type, ReadingTypes | BeforeHeader) => {
                let name = element
                    .get_required_field("type definition", "name")?
                    .require_known_symbol("type name")?;
                self.schema_items.push(SchemaItem::Type(
                    name.into(),
                    TypeDefinition::try_read(&element, &self.reader_context)?,
                ));
                ReadingTypes
            }
            (Footer, ReadingTypes | BeforeHeader) => {
                self.schema_items
                    .push(SchemaItem::Footer(SchemaFooter::try_read(
                        &element,
                        &self.reader_context,
                    )?));
                AfterFooter
            }
            (syntax, state) => {
                invalid_schema_2!(&element, "illegal {syntax:?} while {state:?}: {element}")?
            }
        };
        Ok(())
    }

    pub fn close(self) -> ReadResult<SchemaDocument> {
        self.error_collector.into_result_with(SchemaDocument::new::<V>(self.schema_items))
    }
}

#[cfg(test)]
mod tests {
    use ion_rs::Element;
    use crate::{ISL_1_0, ISL_2_0};
    use crate::loader::schema_reader::SchemaReader;

    // With/without version marker
    // With/without header
    // With/without types
    // With/without open content
    // With/without footer

    // Things in wrong order


    #[test]
    fn read_isl_1_0() {
        let mut schema_document = Element::read_all(r"
        $ion_schema_1_0
        type::{
          name: foo,
        }
        ").unwrap().into_iter();
        let mut reader = SchemaReader::<ISL_1_0>::new();
        reader.consume(schema_document.next().unwrap());
        reader.consume(schema_document.next().unwrap());
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
        let mut schema_document = Element::read_all(r#"
        $ion_schema_2_0
        type::{ named: foo }
        type::{ name: bar, valid_values: range::[a, "z"] }
        $ion_schema_2_0
        "#).unwrap().into_iter();
        let mut reader = SchemaReader::<ISL_2_0>::new();
        schema_document.for_each(|el| reader.consume(el));
        let schema = reader.close();
        assert_reader_err!(schema)
    }
}
