// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{LoaderContext, ReadFromIsl, WriteAsIsl, WriteContext};
use crate::isl::isl_type_reference::NullabilityModifier;
use crate::model::type_definition::TypeDefinition;
use crate::model::type_reference::TypeReference;
use crate::model::TypeDefinitionBuilder;
use crate::result::{invalid_schema_error, IonSchemaResult};
use crate::IslVersion;
use ion_rs::{Element, Value, ValueWriter};
use std::marker::PhantomData;

/// Represents the argument for a constraint where that argument is an Ion Schema type.
///
///
#[derive(Debug, Clone, PartialEq)]
pub struct TypeArgument {
    nullability_modifier: NullabilityModifier,
    kind: TypeArgumentKind,
}

#[derive(Debug, Clone, PartialEq)]
enum TypeArgumentKind {
    TypeReference(TypeReference),
    InlineType(TypeDefinition),
}

impl<V: IslVersion> WriteAsIsl<V> for TypeArgument
where
    TypeDefinition: WriteAsIsl<V>,
{
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        let writer = match (self.nullability_modifier, V::MAJOR_MINOR) {
            (NullabilityModifier::Nothing, _) => writer.with_annotations([] as [&'static str; 0]),
            (NullabilityModifier::NullOr, (1, _)) => todo!("Implement down-conversion to ISL 2.0 by wrapping in `type: $any, any_of: [$null,  T]`"),
            (NullabilityModifier::NullOr, (2, _)) => writer.with_annotations(["$null_or"]),
            (NullabilityModifier::Nullable, (1, _)) => writer.with_annotations(["nullable"]),
            (NullabilityModifier::Nullable, (2, _)) => return invalid_schema_error("'nullable::' is not valid for Ion Schema 2.0 and higher"),
            _ => unreachable!(),
        }?;

        match &self.kind {
            TypeArgumentKind::TypeReference(t) => TypeReference::write_as_isl(t, writer, ctx),
            TypeArgumentKind::InlineType(t) => TypeDefinition::write_as_isl(t, writer, ctx),
        }?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct TypeArgumentBuilder<V: IslVersion> {
    _version: PhantomData<V>,
    nullability_modifier: NullabilityModifier,
    kind: TypeArgumentKind,
}
impl<V: IslVersion> TypeArgumentBuilder<V> {
    pub fn type_reference<T: Into<TypeReference>>(type_reference: T) -> Self {
        TypeArgumentBuilder {
            _version: PhantomData,
            nullability_modifier: NullabilityModifier::Nothing,
            kind: TypeArgumentKind::TypeReference(type_reference.into()),
        }
    }

    pub fn inline(inline: TypeDefinitionBuilder<V>) -> Self {
        TypeArgumentBuilder {
            _version: PhantomData,
            nullability_modifier: NullabilityModifier::Nothing,
            kind: TypeArgumentKind::InlineType(inline.build()),
        }
    }

    pub fn nullable(mut self, b: bool) -> Self {
        let new_nullability = match (b, V::MAJOR_MINOR) {
            (false, _) => NullabilityModifier::Nothing,
            (_, (1, _)) => NullabilityModifier::Nullable,
            (_, _) => NullabilityModifier::NullOr,
        };
        self.nullability_modifier = new_nullability;
        self
    }

    pub(crate) fn build(self) -> TypeArgument {
        TypeArgument {
            nullability_modifier: self.nullability_modifier,
            kind: self.kind,
        }
    }
}

impl<V: IslVersion> ReadFromIsl<V> for TypeArgumentBuilder<V>
where
    TypeDefinitionBuilder<V>: ReadFromIsl<V>,
{
    fn try_read(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Self> {
        let nullability_modifier = ReadFromIsl::try_read(ion, ctx)?;
        let kind = match ion.value() {
            Value::Symbol(s) => TypeArgumentKind::TypeReference(s.expect_text()?.into()),
            Value::Struct(s) => {
                if let Some(id) = s.get("id") {
                    TypeArgumentKind::TypeReference(TypeReference::try_read(ion, ctx)?)
                } else {
                    let ty: TypeDefinitionBuilder<V> = ReadFromIsl::try_read(ion, ctx)?;
                    TypeArgumentKind::InlineType(ty.build())
                }
            }
            other => invalid_schema_error(format!("not a type argument: {other}"))?,
        };
        Ok(TypeArgumentBuilder {
            nullability_modifier,
            kind,
            _version: PhantomData,
        })
    }
}
