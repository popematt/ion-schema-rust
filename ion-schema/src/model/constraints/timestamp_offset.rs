// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::TypeDefinitionBuilder;
use crate::result::{IonSchemaError, IonSchemaResult};
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for TimestampOffset {
    const CONSTRAINT_NAME: &'static str = "timestamp_offset";
}

/// Represents the `timestamp_offset` constraint (ref.  [ISL 1.0],[ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#timestamp_offset
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#timestamp_offset
#[derive(Debug, Clone, PartialEq)]
pub struct TimestampOffset {
    // TODO: Replace this Vec with a Set, or sort the vec upon construction
    values: Vec<TimestampOffsetValue>,
}

impl TimestampOffset {
    pub(crate) fn new<T: IntoIterator<Item = TimestampOffsetValue>>(values: T) -> Self {
        let values = values.into_iter().collect();
        Self { values }
    }

    pub fn values(&self) -> impl Iterator<Item = &TimestampOffsetValue> {
        self.values.iter()
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn timestamp_offset<T: IntoIterator<Item = TimestampOffsetValue>>(self, values: T) -> Self {
        let constraint = TimestampOffset::new(values);
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for TimestampOffset {
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

impl<V: IslVersion> WriteAsIsl<V> for TimestampOffset {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl<V: IslVersion> ReadConstraint<V> for TimestampOffset {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

/// Represent a timestamp offset
/// Known timestamp offset value is stored in minutes.
#[derive(Debug, Clone, PartialEq)]
pub struct TimestampOffsetValue {
    minutes: Option<i16>,
}
impl TimestampOffsetValue {
    const UNKNOWN: TimestampOffsetValue = TimestampOffsetValue { minutes: None };
    const UTC: TimestampOffsetValue = TimestampOffsetValue { minutes: Some(0) };

    /// The total number of minutes
    fn minutes(&self) -> Option<i16> {
        self.minutes
    }
}

impl TryFrom<&str> for TimestampOffsetValue {
    type Error = IonSchemaError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        todo!()
    }
}
