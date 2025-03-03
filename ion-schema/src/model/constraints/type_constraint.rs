// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ReadFromIsl, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::ConstraintName;
use crate::model::type_argument::{TypeArgument, TypeArgumentBuilder};
use crate::model::TypeDefinitionBuilder;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

/// Represents the `type` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// To avoid name conflicts with the keyword `type` and confusion with other Ion Schema concepts
/// related to types, any types or functions pertaining to this constraint must have the word
/// "constraint" in the name.
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#type
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#type
#[derive(Debug, PartialEq, Clone)]
pub struct TypeConstraint {
    pub(crate) type_argument: TypeArgument,
}

impl TypeConstraint {
    pub(crate) fn new<T: Into<TypeArgument>>(type_argument: T) -> Self {
        Self {
            type_argument: type_argument.into(),
        }
    }

    pub fn type_argument(&self) -> &TypeArgument {
        &self.type_argument
    }
}

impl ConstraintName for TypeConstraint {
    const CONSTRAINT_NAME: &'static str = "type";
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn type_constraint(mut self, type_argument: TypeArgument) -> Self {
        let constraint = TypeConstraint { type_argument };
        self.constraints.push(constraint.into());
        self
    }
}

impl ValidateInternal for TypeConstraint {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        todo!("Implement this once the ValidationContext allows looking up a type")
    }
}

impl<V: IslVersion> WriteAsIsl<V> for TypeConstraint
where
    TypeArgument: WriteAsIsl<V>,
{
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        TypeArgument::write_as_isl(self.type_argument(), writer, ctx)
    }
}

impl<V: IslVersion> ReadFromIsl<V> for TypeConstraint
where
    TypeArgumentBuilder<V>: ReadFromIsl<V>,
{
    fn try_read(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Self> {
        Ok(TypeConstraint {
            type_argument: TypeArgumentBuilder::try_read(ion, ctx)?.build(),
        })
    }
}

// TODO: WriteAsIsl Tests, once more of the supporting pieces around types have been implemented
// TODO: ReadFromIsl Tests, once more of the supporting pieces around types have been implemented
