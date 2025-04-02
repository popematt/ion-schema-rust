// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::loader::schema_reader::IslSyntaxKind::{Footer, OpenContent, Type};
use crate::loader::schema_reader::SchemaReaderState::BeforeHeader;
use crate::loader::{ReadFromIsl, ReaderContext};
use crate::model::{SchemaDocument, SchemaFooter, SchemaHeader, SchemaItem, TypeDefinition,};
use crate::result::{invalid_schema, IonSchemaResult};
use crate::{IslVersion, ISL_VERSION_MARKER_REGEX, RESERVED_WORD_REGEX};
use ion_rs::Element;

/// A helper type for encapsulating the state needed to read a [`SchemaDocument`].
pub(super) struct SchemaReader<V: IslVersion> {
    state: SchemaReaderState,
    schema_items: Vec<SchemaItem>,
    reader_context: ReaderContext<V>,
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
        }
    }

    pub fn consume(&mut self, element: Element) -> IonSchemaResult<()> {
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
                    let first_annotation = ann.iter().next().unwrap().expect_text()?;
                    if RESERVED_WORD_REGEX.is_match(first_annotation) {
                        invalid_schema!("extra content cannot be annotated with reserved word: {element}")?;
                    }
                }
                if V::MAJOR_MINOR != (1, 0) && ann.is_empty() {
                    if let Some(maybe_isl_version) = element.as_symbol().and_then(|e| e.text()) {
                        if ISL_VERSION_MARKER_REGEX.is_match(maybe_isl_version) {
                            invalid_schema!("unexpected Ion Schema version marker: {element}")?;
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
                let Some(name_element) = element.expect_struct()?.get("name") else {
                    invalid_schema!("top-level type definition missing name: {element}")?
                };
                let name = name_element.expect_symbol()?.expect_text()?;
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
                invalid_schema!("illegal {syntax:?} while {state:?}: {element}")?
            }
        };
        Ok(())
    }

    pub fn close(self) -> SchemaDocument {
        SchemaDocument::new::<V>(self.schema_items)
    }
}
