// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

mod document_authority;
mod load_fn;
mod read_from_isl;
mod schema_reader;

use crate::model::SchemaDocument;
use crate::result::{
    InvalidSchemaError, InvalidSchemaErrorCollector, IonSchemaError, IonSchemaResult,
};
use load_fn::*;

use crate::resolver::{resolve, resolve_internal, ResolvedSchema};
pub use document_authority::*;
pub(crate) use read_from_isl::*;
// use crate::test_harness::err;

pub trait SchemaLoader: DocumentAuthority {
    /// Locates and reads one or more schemas, returning a [`SchemaDocument`] for each requested
    /// schema id and any dependencies of those schemas.
    ///
    /// For validation use cases, [`self.load_schema`] will be sufficient.
    fn read_all(
        &self,
        schema_ids: impl IntoIterator<Item = impl Into<String>>,
    ) -> IonSchemaResult<impl Iterator<Item = (String, SchemaDocument)>>;

    /// Loads (locates, reads, and resolves) a schema and any necessary dependencies, returning
    /// the requested schema as a [`ResolvedSchema`].
    fn load_schema(&self, id: impl Into<String>) -> IonSchemaResult<ResolvedSchema>;
}

impl<T: DocumentAuthority> SchemaLoader for T {
    fn read_all(
        &self,
        schema_ids: impl IntoIterator<Item = impl Into<String>>,
    ) -> IonSchemaResult<impl Iterator<Item = (String, SchemaDocument)>> {
        load(schema_ids, self)
            .map(|it| it.into_iter())
            .map_err(|(partial, e)| e.into())
    }

    fn load_schema(&self, id: impl Into<String>) -> IonSchemaResult<ResolvedSchema> {
        let id = id.into();

        let mut error_collector = InvalidSchemaErrorCollector::default();

        let loaded = match load([&id], self) {
            Ok(schemas) => schemas,
            Err((Some(schemas), err)) => {
                error_collector.push_err(err);
                schemas
            }
            Err((None, err)) => return Err(err.into()),
        };

        let resolved_schemas = error_collector.ok_or_push_err(resolve_internal(loaded));
        error_collector
            .into_result_with(resolved_schemas)
            .map(|resolved_schemas| {
                resolved_schemas
                    .unwrap()
                    .into_iter()
                    .find_map(|(k, v)| if k == id { Some(v) } else { None })
                    .unwrap()
            })
            .map_err(IonSchemaError::InvalidSchemaError)
    }
}

#[cfg(test)]
mod tests {
    use crate::loader::SchemaLoader;
    use std::collections::HashMap;

    #[test]
    fn foo() {
        let mut authority = HashMap::new();
        authority.insert(
            "foo.isl",
            r#"
        $ion_schema_2_0
        type::{
          name: foo_type,
          type: { id: "bar.isl", type: bar_type }
        }
        type::{
          name: foo_type,
          type: { id: "bar.isl", type: bar_type }
        }
        type::{
          name: baz_type,
          type: "quux",
        }
        "#,
        );
        authority.insert(
            "bar.isl",
            r#"
        $ion_schema_2_0
        type::{
          name: bar_type,
          type: { id: "foo.isl", type: foo_type }
        }
        "#,
        );

        let result = authority.load_schema("foo.isl");

        if let Err(e) = result {
            println!("{e}")
        }
    }
}
