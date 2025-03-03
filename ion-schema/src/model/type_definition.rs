// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ReadFromIsl, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{AnnotationsV2Simple, AnyConstraint, TypeConstraint};
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder, ISL_1_0, ISL_2_0};
use ion_rs::{Element, IonData, IonType, StructWriter, Symbol, ValueWriter};
use std::marker::PhantomData;
use std::ops::ControlFlow;

/// A TypeDefinition is a set of constraints and (optionally) additional user content.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinition {
    // FIXME: Replace these with bags or use a set and decide that this implementation of Ion
    //        Schema does not preserve duplicate name/value pairs for open content. That
    //        may be an acceptable limitation.
    constraints: Vec<AnyConstraint>,
    open_content: Vec<(Symbol, IonData<Element>)>,
}

impl TypeDefinition {
    pub(crate) fn new(
        constraints: Vec<AnyConstraint>,
        open_content: Vec<(Symbol, IonData<Element>)>,
    ) -> Self {
        Self {
            constraints: constraints.into_iter().collect(),
            open_content: open_content.into_iter().collect(),
        }
    }

    pub fn constraints(&self) -> impl Iterator<Item = &AnyConstraint> {
        self.constraints.iter()
    }

    pub fn open_content(&self) -> impl Iterator<Item = (&Symbol, &Element)> {
        self.open_content
            .iter()
            .map(|(name, value)| (name, value.as_ref()))
    }
}

/// A version-safe builder for Ion Schema type definitions.
#[derive(Debug, Clone)]
pub struct TypeDefinitionBuilder<V: IslVersion> {
    pub(crate) constraints: Vec<AnyConstraint>,
    open_content: Vec<(Symbol, IonData<Element>)>,
    isl_version: PhantomData<V>,
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub(crate) fn new() -> Self {
        TypeDefinitionBuilder {
            constraints: vec![],
            open_content: vec![],
            isl_version: PhantomData::<V>,
        }
    }

    // This is intentionally pub(crate) because we don't want people to build an ISL 1.0 type and
    // inline it into an ISL 2.0 type. Constraints that have type arguments should accept a
    // TypeDefinitionBuilder or a TypeReference.
    pub(crate) fn build(self) -> TypeDefinition {
        TypeDefinition::new(self.constraints, self.open_content)
    }

    pub fn with_open_content<S: Into<Symbol>, E: Into<Element>>(
        mut self,
        field_name: S,
        value: E,
    ) -> Self {
        self.open_content
            .push((field_name.into(), IonData::from(value.into())));
        self
    }
}

impl ValidateInternal for TypeDefinition {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &'top IonSchemaElement,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        for c in &self.constraints {
            c.validate_internal(value, ctx, recorder)?;
        }
        ControlFlow::Continue(())
    }
}

impl<V: IslVersion> WriteAsIsl<V> for TypeDefinition
where
    AnyConstraint: WriteAsIsl<V>,
{
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        let mut struct_writer = writer.struct_writer()?;

        for c in &self.constraints {
            let value_writer = struct_writer.field_writer(c.constraint_keyword());
            c.write_as_isl(value_writer, ctx)?;
        }
        // Can't use `write_all` because `IonData` doesn't implement `WriteAsIon`
        for (k, v) in &self.open_content {
            struct_writer.write(k, v.as_ref())?;
        }
        struct_writer.close()?;
        Ok(())
    }
}

impl ReadFromIsl<ISL_1_0> for TypeDefinitionBuilder<ISL_1_0> {
    fn try_read(ion: &Element, ctx: &LoaderContext<ISL_1_0>) -> IonSchemaResult<Self> {
        let struct_ = ion.expect_struct()?;
        let mut builder = TypeDefinitionBuilder::new();

        for (name, value) in struct_.fields() {
            match name.expect_text()? {
                "type" => builder
                    .constraints
                    .push(TypeConstraint::try_read(value, ctx)?.into()),
                // TODO: Other constraints
                // TODO: ignore "name" iff this is a top level type definition
                other => builder
                    .open_content
                    .push((name.clone(), IonData::from(value.clone()))),
            }
        }
        Ok(builder)
    }
}

impl ReadFromIsl<ISL_2_0> for TypeDefinitionBuilder<ISL_2_0> {
    fn try_read(ion: &Element, ctx: &LoaderContext<ISL_2_0>) -> IonSchemaResult<Self> {
        let struct_ = ion.expect_struct()?;
        let mut builder = TypeDefinitionBuilder::new();

        for (name, value) in struct_.fields() {
            match name.expect_text()? {
                "annotations" => {
                    if value.ion_type() == IonType::List {
                        builder
                            .constraints
                            .push(AnnotationsV2Simple::try_read(value, ctx)?.into())
                    } else {
                        todo!("standard syntax")
                    }
                }
                "type" => builder
                    .constraints
                    .push(TypeConstraint::try_read(value, ctx)?.into()),
                // TODO: Other constraints
                // TODO: ignore "name" iff this is a top level type definition
                other => {
                    // TODO: Check to make sure that this is valid open content before adding.
                    builder
                        .open_content
                        .push((name.clone(), IonData::from(value.clone())))
                }
            }
        }
        Ok(builder)
    }
}
