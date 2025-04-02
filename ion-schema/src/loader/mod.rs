// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

mod document_authority;
mod load_fn;
mod read_from_isl;
mod schema_reader;

use crate::model::SchemaDocument;
use crate::result::IonSchemaResult;
use crate::IslVersion;
use load_fn::*;
use std::borrow::Borrow;
use std::fmt::Debug;

use crate::resolver::{resolve, ResolvedSchema};
pub use document_authority::*;
pub(crate) use read_from_isl::*;

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
        load(schema_ids.into_iter().map(|s| s.into()).collect(), self)
    }

    fn load_schema(&self, id: impl Into<String>) -> IonSchemaResult<ResolvedSchema> {
        let id = id.into();
        let mut resolved_schemas = resolve(load(vec![id.clone()], self)?)?;
        Ok(resolved_schemas
            .find_map(|(k, v)| if k == id { Some(v) } else { None })
            .unwrap())
    }
}
