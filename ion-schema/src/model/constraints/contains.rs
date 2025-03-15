// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::TypeDefinitionBuilder;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for Contains {
    const CONSTRAINT_NAME: &'static str = "contains";
}

/// Represents the `contains` constraint (ref.  [ISL 1.0],[ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#contains
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#contains
#[derive(Debug, PartialEq, Clone)]
pub struct Contains {
    values: Vec<Element>,
}

impl Contains {
    pub(crate) fn new<T: IntoIterator<Item = Element>>(values: T) -> Self {
        Self {
            values: values.into_iter().collect(),
        }
    }

    pub fn values(&self) -> &[Element] {
        &self.values
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn contains<T: IntoIterator<Item = Element>>(self, values: T) -> Self {
        let constraint = Contains::new(values);
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for Contains {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &'top IonSchemaElement,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        todo!()
    }
}

impl<V: IslVersion> WriteAsIsl<V> for Contains {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl<V: IslVersion> ReadConstraint<V> for Contains {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}
