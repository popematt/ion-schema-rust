// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::loader::schema_reader::read_isl;
use crate::loader::{DocumentAuthority, ReadFromIsl, ReadResult, ReadResultWithPartialSuccess};
use crate::model::{SchemaDocument, SchemaFooter, SchemaHeader, TypeDefinition, TypeReference};
use crate::resolver::{resolve, ResolvedSchema, TypeRefWalker};
use crate::result::{
    invalid_schema_2, InvalidSchemaError, InvalidSchemaErrorCollector, IonSchemaResult,
    IslSourceLocation,
};
use std::collections::{BTreeSet, HashMap};

pub(super) fn load(
    schema_ids: impl IntoIterator<Item = impl Into<String>>,
    authority: &impl DocumentAuthority,
) -> ReadResultWithPartialSuccess<HashMap<String, SchemaDocument>> {
    let mut unloaded: Vec<_> = schema_ids.into_iter().map(|it| it.into()).collect();
    let mut loaded_schemas: HashMap<String, SchemaDocument> = HashMap::new();

    let mut error_collector = InvalidSchemaErrorCollector::default();

    while let Some(id) = unloaded.pop() {
        if loaded_schemas.contains_key(&id) {
            continue;
        }

        let content = match authority.elements(&id) {
            Ok(Some(content)) => content,
            Ok(None) => {
                error_collector.push_err(invalid_schema_2!(
                    &IslSourceLocation::NONE,
                    schema = id,
                    "schema not found"
                ));
                continue;
            }
            Err(e) => {
                error_collector.push_err(invalid_schema_2!(
                    &IslSourceLocation::NONE,
                    schema = id,
                    "schema document is not valid Ion: {e}"
                ));
                continue;
            }
        };

        let mut schema_document = match read_isl(content) {
            Ok(schema_document) => schema_document,
            Err((schema_document, err)) => {
                error_collector.push_err(err.with_schema_id(&id));
                match schema_document {
                    None => {
                        continue;
                    }
                    Some(schema_document) => schema_document,
                }
            }
        };

        // let Some(mut schema_document): Option<SchemaDocument> = error_collector.ok_or_push_err(schema_document) else {
        //     continue;
        // };

        // Look for any imports in the schema and add them to the "unloaded" queue, if needed.
        if let Some(header) = schema_document.header() {
            header.imports().for_each(|import| {
                let schema_id = import.schema_id().to_owned();
                if !loaded_schemas.contains_key(&schema_id) {
                    unloaded.push(schema_id);
                }
            });
        }
        schema_document.walk(&mut |type_ref: &mut TypeReference| {
            if let Some(schema_id) = type_ref.schema_id() {
                if !loaded_schemas.contains_key(schema_id) {
                    unloaded.push(schema_id.to_owned());
                }
            }
        });
        loaded_schemas.insert(id, schema_document);
    }
    error_collector.into_partial_result_with(loaded_schemas)
}
