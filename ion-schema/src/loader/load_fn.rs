// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::loader::schema_reader::SchemaReader;
use crate::loader::{DocumentAuthority, ReadFromIsl};
use crate::model::{
    SchemaDocument, SchemaFooter, SchemaHeader, TypeDefinition, TypeReference,
};
use crate::resolver::TypeRefWalker;
use crate::result::{invalid_schema_error, unresolvable_schema_error, IonSchemaResult};
use crate::{IslVersion, ISL_1_0, ISL_2_0, ISL_VERSION_MARKER_REGEX};
use ion_rs::Element;
use std::collections::{BTreeSet, HashMap};

pub(super) fn load(
    schema_ids: Vec<String>,
    authority: &impl DocumentAuthority,
) -> IonSchemaResult<impl Iterator<Item = (String, SchemaDocument)>> {
    let mut unloaded: BTreeSet<_> = schema_ids.into_iter().collect();
    let mut loaded_schemas: HashMap<String, SchemaDocument> = HashMap::new();

    while let Some(id) = unloaded.pop_first() {
        if loaded_schemas.contains_key(&id) {
            continue;
        }

        let Some(content) = authority.elements(&id)? else {
            return unresolvable_schema_error(format!("Could not find schema: {id}"));
        };
        let mut schema_document = read_schema_with_unknown_version(content.into_iter())?;

        if let Some(header) = schema_document.header() {
            header.imports().for_each(|import| {
                let schema_id = import.schema_id().to_owned();
                if !loaded_schemas.contains_key(&schema_id) {
                    unloaded.insert(schema_id);
                }
            });
        }
        schema_document.walk(&mut |type_ref: &mut TypeReference| {
            if let Some(schema_id) = type_ref.schema_id() {
                if !loaded_schemas.contains_key(schema_id) {
                    unloaded.insert(schema_id.to_owned());
                }
            }
        })
    }
    Ok(loaded_schemas.into_iter())
}

fn read_schema_with_unknown_version(mut isl: impl Iterator<Item = Element>) -> IonSchemaResult<SchemaDocument> {
    // Read until we find an ISL version marker, a schema header, schema footer, or a schema type
    // Store any open content until then
    let mut open_content = vec![];
    while let Some(element) = isl.next() {
        let ann = element.annotations();
        if ann.is_empty() {
            // Check for a version marker
            if let Some(symbol) = element.as_symbol() {
                let maybe_isl_version = symbol.expect_text()?;
                if ISL_VERSION_MARKER_REGEX.is_match(maybe_isl_version) {
                    return match maybe_isl_version {
                        ISL_1_0::VERSION_MARKER_TEXT => {
                            read_schema_with_known_version::<ISL_1_0>(open_content, None, isl)
                        }
                        ISL_2_0::VERSION_MARKER_TEXT => {
                            read_schema_with_known_version::<ISL_2_0>(open_content, None, isl)
                        }
                        other => invalid_schema_error(format!(
                            "unknown/unsupported Ion Schema Version: {other}"
                        )),
                    };
                }
            }
        } else if ann.contains("type")
            || ann.contains("schema_header")
            || ann.contains("schema_footer")
        {
            // If we see a header, footer, or type before a version marker, then it's ISL 1.0
            return read_schema_with_known_version::<ISL_1_0>(open_content, Some(element), isl);
        }
        open_content.push(element)
    }
    // If we still haven't found any ISL syntax, then it's all open content. Weird, but okay.
    read_schema_with_known_version::<ISL_1_0>(open_content, None, isl)
}

fn read_schema_with_known_version<V: IslVersion>(
    open_content: Vec<Element>,
    next_element: Option<Element>,
    isl: impl Iterator<Item = Element>,
) -> IonSchemaResult<SchemaDocument>
where
    TypeDefinition: ReadFromIsl<V>,
    SchemaHeader: ReadFromIsl<V>,
    SchemaFooter: ReadFromIsl<V>,
{
    let mut reader = SchemaReader::<V>::new();
    for item in open_content {
        reader.consume(item)?;
    }
    if let Some(item) = next_element {
        reader.consume(item)?;
    }
    for item in isl {
        reader.consume(item)?;
    }
    Ok(reader.close())
}
