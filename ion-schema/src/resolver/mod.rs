// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::model::SchemaDocument;

mod resolve_fn;
mod resolved_schema;
mod type_ref_visitor;

pub use resolve_fn::resolve;
pub use resolve_fn::unresolve;
pub use resolved_schema::*;
pub(crate) use type_ref_visitor::*;

/// A schema index and type index.
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct TypeCoordinates(usize, usize);
impl TypeCoordinates {
    #[cfg(test)]
    pub(crate) fn new(s_idx: usize, t_idx: usize) -> Self {
        TypeCoordinates(s_idx, t_idx)
    }

    fn schema_idx(&self) -> usize {
        self.0
    }
    fn type_idx(&self) -> usize {
        self.1
    }
}

#[derive(Debug)]
pub(crate) struct SchemaStore {
    schemas: Vec<(String, SchemaDocument)>,
}

impl SchemaStore {
    pub(crate) fn get_schema(&self, schema_idx: usize) -> &SchemaDocument {
        &self.schemas[schema_idx].1
    }
}
