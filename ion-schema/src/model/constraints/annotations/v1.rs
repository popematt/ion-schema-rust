// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::*;
use crate::model::constraints::annotations::AnnotationsVariant;
use crate::model::constraints::{Annotations, ReadConstraint};
use crate::model::TypeDefinitionBuilder;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, ISL_1_0, ISL_2_0};
use ion_rs::{Element, Symbol, ValueWriter};
use std::ops::ControlFlow;

/// Represents a single annotation for the [`annotations`](AnnotationsV1) constraint in Ion Schema 1.0.
#[derive(Clone, Debug, PartialEq)]
pub enum AnnotationV1<T: Into<Symbol>> {
    Optional(T),
    Required(T),
}

pub type AnnotationSymbol = AnnotationV1<Symbol>;

/// Represents the [annotations] constraint in Ion Schema 1.0
///
/// [annotations]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#annotations
#[derive(Debug, PartialEq, Clone)]
pub struct AnnotationsV1 {
    annotations: Vec<AnnotationSymbol>,
    closed: bool,
    ordered: bool,
}

impl AnnotationsV1 {
    pub fn annotations(&self) -> &[AnnotationSymbol] {
        &self.annotations
    }

    pub fn closed(&self) -> bool {
        self.closed
    }

    pub fn ordered(&self) -> bool {
        self.ordered
    }

    pub(super) fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: FnMut(IonSchemaElement<'top>, String) -> ControlFlow<()>,
    {
        todo!()
    }
}

pub trait IntoAnnotationSymbol {
    fn into_annotation_symbol(self) -> AnnotationSymbol;
}

impl<S: Into<Symbol>> IntoAnnotationSymbol for AnnotationV1<S> {
    fn into_annotation_symbol(self) -> AnnotationSymbol {
        match self {
            AnnotationV1::Optional(s) => AnnotationV1::Optional(s.into()),
            AnnotationV1::Required(s) => AnnotationV1::Required(s.into()),
        }
    }
}

pub trait AnnotationsV1Source {
    /// Configures whether the annotations are closed.
    ///
    /// When set to `true`, only the specified annotations are allowed.
    /// When `false`, additional annotations beyond those specified are permitted.
    ///
    /// # Arguments
    /// * `yes` - If true, restricts to only specified annotations
    ///
    /// # Returns
    /// * An implementation of `AnnotationsV1Source` with the closed setting configured
    fn closed(self, yes: bool) -> impl AnnotationsV1Source;

    /// Configures whether the annotations must maintain their specified order.
    ///
    /// When set to `true`, annotations must appear in the same order as specified.
    /// When `false`, annotations can appear in any order.
    ///
    /// # Arguments
    /// * `yes` - If true, enforces the order of annotations
    ///
    /// # Returns
    /// * An implementation of `AnnotationsV1Source` with the ordered setting configured
    fn ordered(self, yes: bool) -> impl AnnotationsV1Source;

    /// Builds and returns the final AnnotationsV1 instance.
    ///
    /// # Returns
    /// * The constructed `AnnotationsV1` with all configured settings
    fn build(self) -> AnnotationsV1;
}

#[derive(Default, Debug, Clone)]
struct AnnotationsV1Builder {
    annotations: Vec<AnnotationSymbol>,
    closed: bool,
    ordered: bool,
}
impl AnnotationsV1Builder {
    fn from_iter<T: IntoAnnotationSymbol, I: IntoIterator<Item = T>>(iter: I) -> Self {
        AnnotationsV1Builder {
            annotations: iter
                .into_iter()
                .map(|i| i.into_annotation_symbol())
                .collect(),
            closed: false,
            ordered: false,
        }
    }
}

impl AnnotationsV1Source for AnnotationsV1Builder {
    fn closed(mut self, yes: bool) -> impl AnnotationsV1Source {
        self.closed = yes;
        self
    }

    fn ordered(mut self, yes: bool) -> impl AnnotationsV1Source {
        self.ordered = yes;
        self
    }

    fn build(self) -> AnnotationsV1 {
        AnnotationsV1 {
            annotations: self.annotations,
            closed: self.closed,
            ordered: self.ordered,
        }
    }
}

impl<I> AnnotationsV1Source for I
where
    I: IntoIterator,
    I::Item: IntoAnnotationSymbol,
{
    fn closed(self, yes: bool) -> impl AnnotationsV1Source {
        let b: AnnotationsV1Builder = AnnotationsV1Builder::from_iter(self);
        b.closed(yes)
    }

    fn ordered(self, yes: bool) -> impl AnnotationsV1Source {
        let b: AnnotationsV1Builder = AnnotationsV1Builder::from_iter(self);
        b.ordered(yes)
    }

    fn build(self) -> AnnotationsV1 {
        let b: AnnotationsV1Builder = AnnotationsV1Builder::from_iter(self);
        b.build()
    }
}

impl TypeDefinitionBuilder<ISL_1_0> {
    /// Adds an annotations constraint to the type definition being built.
    ///
    /// This method allows adding annotations validation rules to an Ion Schema 1.0 type definition.
    /// It accepts any source that implements the `AnnotationsV1Source` trait to configure
    /// the annotation constraints.
    ///
    /// # Arguments
    /// * `source` - The source of annotation constraints implementing `AnnotationsV1Source`
    ///
    /// # Returns
    /// * Returns `Self` (the builder) to allow for method chaining
    ///
    /// # Examples
    /// ```
    /// # use ion_schema::ISL_1_0;
    /// # use ion_schema::model::constraints::AnnotationsV1Source;
    /// use ion_schema::model::TypeDefinition;
    /// use ion_schema::model::constraints::AnnotationV1::*;
    /// let type_def = TypeDefinition::builder::<ISL_1_0>()
    ///     .annotations(
    ///         [Required("foo"), Optional("bar")]
    ///             .closed(true)
    ///     )
    ///     .build();
    /// ```
    pub fn annotations<T: AnnotationsV1Source>(self, source: T) -> Self {
        let constraint = Annotations {
            variant: AnnotationsVariant::V1(source.build()),
        };
        self.with_constraint(constraint.into())
    }
}

impl WriteAsIsl<ISL_1_0> for AnnotationsV1 {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<ISL_1_0>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

// Default implementation returns Err to indicate that this is unsupported.
impl WriteAsIsl<ISL_2_0> for AnnotationsV1 {
    // TODO: Implement up-conversion to ISL 2.0 when possible.
}

impl ReadConstraint<ISL_1_0> for AnnotationsV1 {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_1_0>,
    ) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

impl ReadConstraint<ISL_2_0> for AnnotationsV1 {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builder() {
        use AnnotationV1::*;

        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .annotations([Optional("a"), Required("b")].closed(true).ordered(true))
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![Annotations {
                variant: AnnotationsVariant::V1(AnnotationsV1 {
                    annotations: vec![Optional("a".into()), Required("b".into()),],
                    closed: true,
                    ordered: true,
                })
            }
            .into()]
        )
    }
}
