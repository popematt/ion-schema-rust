// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{ValidateInternal, ValidationContext, WriteAsIsl, WriteContext};
use crate::model::constraints::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::ValueWriter;
use std::ops::ControlFlow;

macro_rules! any_constraint {
    ($($name:ident,)+) => {
        #[derive(Debug, PartialEq, Clone)]
        pub enum AnyConstraint {
            $($name($name)),*
        }

        $(
        impl From<$name> for AnyConstraint {
            fn from(value: $name) -> Self { AnyConstraint::$name(value) }
        }
        )+

        // Any traits that should be implemented by delegating to the enum variants can be added here.
        impl ValidateInternal for AnyConstraint {
            fn validate_internal<'top: 'call, 'call, R>(
                &'top self,
                value: &'top IonSchemaElement<'top>,
                ctx: &ValidationContext,
                recorder: &'call mut R,
            ) -> ControlFlow<()>
            where
                R: ViolationRecorder<'top>,
            {
                match self {
                    $(AnyConstraint::$name(constraint) => constraint.validate_internal(value, ctx, recorder),
                    )+
                }
            }
        }

        impl<V: IslVersion> WriteAsIsl<V> for AnyConstraint
        where
            $(
            $name: WriteAsIsl<V>,)+
        {
            fn write_as_isl<W: ValueWriter>(&self, writer: W, ctx: &WriteContext<V>) -> IonSchemaResult<()> {
                match self {
                    $(AnyConstraint::$name(constraint) => $name::write_as_isl(constraint, writer, ctx),
                    )+
                }
            }
        }

        impl AnyConstraint {
            pub(crate) const fn constraint_keyword(&self) -> &'static str {
                match self {
                    $(AnyConstraint::$name(_) => $name::CONSTRAINT_NAME,
                    )+
                }
            }
        }

        /// An enum over references to all types of constraints. See [`AnyConstraint`].
        #[derive(Debug, PartialEq, Clone)]
        pub enum AnyConstraintRef<'a> {
            $($name(&'a $name)),*
        }

        $(
        impl<'a> From<&'a $name> for AnyConstraintRef<'a> {
            fn from(value: &'a $name) -> Self { AnyConstraintRef::$name(value) }
        }
        )+
    }
}

any_constraint!(
    // AllOf,
    // AnnotationsV1,
    AnnotationsV2Simple,
    // AnnotationsV2Standard,
    // AnyOf,
    // ByteLength,
    // CodepointLength,
    // ContainerLength,
    // Contains,
    // ElementConstraint,
    // Exponent,
    // Fields,
    // FieldNames,
    // Ieee754Float,
    // Not,
    // OneOf,
    // OrderedElements,
    // Precision,
    // Regex,
    // Scale,
    // TimestampOffset,
    TimestampPrecision,
    TypeConstraint,
    Utf8ByteLength,
    // ValidValues,
);
