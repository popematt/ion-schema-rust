// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::constraint::RegexConstraint;
use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::type_argument::TypeArgument;
use crate::model::TypeDefinitionBuilder;
use crate::result::{invalid_schema_error_raw, IonSchemaResult};
use crate::{IonSchemaElement, IslVersion, Versioned, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use regex::RegexBuilder;
use std::marker::PhantomData;
use std::ops::ControlFlow;

impl ConstraintName for Regex {
    const CONSTRAINT_NAME: &'static str = "regex";
}

/// Builder for regular expressions in Ion Schema.
#[derive(Clone, PartialEq, Debug)]
pub struct IonSchemaRegexBuilder<V: IslVersion> {
    // The `V` type parameter allows us to safely add new fields in new Ion Schema
    // versions without breaking compatibility.
    _version: PhantomData<V>,
    case_insensitive: bool,
    multiline: bool,
    pattern: String,
}
impl<V: IslVersion> IonSchemaRegexBuilder<V> {
    pub fn new(pattern: &str) -> Self {
        Self {
            _version: Default::default(),
            case_insensitive: false,
            multiline: false,
            pattern: String::from(pattern),
        }
    }

    pub fn case_insensitive(mut self, yes: bool) -> Self {
        self.case_insensitive = yes;
        self
    }

    pub fn multiline(mut self, yes: bool) -> Self {
        self.multiline = yes;
        self
    }

    pub fn build(self) -> IonSchemaResult<Versioned<Regex, V>> {
        // TODO: Clean up the different IslVersion representations
        let version = match V::MAJOR_MINOR {
            (1, 0) => crate::isl::IslVersion::V1_0,
            (2, 0) => crate::isl::IslVersion::V2_0,
            _ => unreachable!(),
        };

        // Apply ISL specific regex validation
        let pattern = RegexConstraint::convert_to_pattern(self.pattern, version)?;

        // Check that `regex::Regex` won't return any errors with this pattern.
        let regex = RegexBuilder::new(pattern.as_str())
            .build()
            .map_err(|e| invalid_schema_error_raw(format!("Invalid regex: {pattern}")))?;

        let constraint = Regex {
            regex,
            multiline: self.multiline,
            case_insensitive: self.case_insensitive,
        };

        Ok(Versioned::new(constraint))
    }
}

/// Trait for something that can be used to create a new [`IonSchemaRegexBuilder`].
pub trait IonSchemaRegexSource<V: IslVersion>
where
    Self: Sized,
{
    fn to_regex_builder(self) -> IonSchemaRegexBuilder<V>;
    fn build_regex(self) -> IonSchemaResult<Versioned<Regex, V>> {
        self.to_regex_builder().build()
    }
}

impl<V: IslVersion, S: AsRef<str>> IonSchemaRegexSource<V> for S {
    fn to_regex_builder(self) -> IonSchemaRegexBuilder<V> {
        IonSchemaRegexBuilder::new(self.as_ref())
    }
}

impl<V: IslVersion> IonSchemaRegexSource<V> for IonSchemaRegexBuilder<V> {
    fn to_regex_builder(self) -> IonSchemaRegexBuilder<V> {
        self
    }
}

impl<V: IslVersion> IonSchemaRegexSource<V> for Versioned<Regex, V> {
    fn to_regex_builder(self) -> IonSchemaRegexBuilder<V> {
        IonSchemaRegexBuilder {
            _version: Default::default(),
            case_insensitive: self.case_insensitive,
            multiline: self.multiline,
            pattern: self.regex.to_string(),
        }
    }
    fn build_regex(self) -> IonSchemaResult<Versioned<Regex, V>> {
        Ok(self)
    }
}

/// Represents the `regex` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#regex
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#regex
#[derive(Debug, Clone)]
pub struct Regex {
    regex: regex::Regex,
    multiline: bool,
    case_insensitive: bool,
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        // We have to manually implement `PartialEq` because `regex::Regex` does not implement it.
        self.regex.as_str() == other.regex.as_str()
            && self.case_insensitive == other.case_insensitive
            && self.multiline == other.multiline
    }
}

impl Regex {
    fn new<V: IslVersion>(builder: IonSchemaRegexBuilder<V>) -> Self {
        let regex = RegexBuilder::new(builder.pattern.as_str())
            // TODO: See if this would fix https://github.com/amazon-ion/ion-rust/issues/399
            //.crlf(true)
            .case_insensitive(builder.case_insensitive)
            .multi_line(builder.multiline)
            .build()
            .expect("Regex should be valid because it was checked in IonSchemaRegex");

        Self {
            regex,
            multiline: builder.multiline,
            case_insensitive: builder.case_insensitive,
        }
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    /// Adds a `regex` constraint to the type being built.
    ///
    /// This will panic if the input is invalid. For a safe alternative, see [`Self::try_regex`].
    pub fn regex(self, regex: impl IonSchemaRegexSource<V>) -> Self {
        self.with_constraint(Versioned::into_inner(regex.build_regex().unwrap()).into())
    }

    /// Adds a `regex` constraint to the type being built.
    pub fn try_regex(self, regex: impl IonSchemaRegexSource<V>) -> IonSchemaResult<Self> {
        Ok(self.with_constraint(Versioned::into_inner(regex.build_regex()?).into()))
    }
}

impl ValidateInternal for Regex {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        todo!()
    }
}

impl<V: IslVersion> WriteAsIsl<V> for Regex
where
    TypeArgument: WriteAsIsl<V>,
{
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl<V: IslVersion> ReadConstraint<V> for Regex {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::constraints::AnyConstraint;
    use crate::ISL_1_0;

    #[test]
    fn test_builder() {
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new().regex("abc").build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Regex(Regex {
                regex: regex::Regex::new("abc").unwrap(),
                multiline: false,
                case_insensitive: false,
            })]
        );

        let my_regex = IonSchemaRegexBuilder::new("abc")
            .case_insensitive(true)
            .multiline(true)
            .build()
            .unwrap();
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .regex(my_regex)
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Regex(Regex {
                regex: RegexBuilder::new("abc")
                    .case_insensitive(true)
                    .multi_line(true)
                    .build()
                    .unwrap(),
                multiline: true,
                case_insensitive: true,
            })]
        );

        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .regex(
                "abc"
                    .to_regex_builder()
                    .case_insensitive(true)
                    .multiline(true),
            )
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Regex(Regex {
                regex: RegexBuilder::new("abc")
                    .case_insensitive(true)
                    .multi_line(true)
                    .build()
                    .unwrap(),
                multiline: true,
                case_insensitive: true,
            })]
        )
    }

    #[test]
    fn test_partial_eq() {
        let regex_constraint = Regex {
            regex: regex::Regex::new("abc").unwrap(),
            multiline: true,
            case_insensitive: true,
        };

        assert_eq!(
            regex_constraint,
            Regex {
                regex: regex::Regex::new("abc").unwrap(),
                multiline: true,
                case_insensitive: true,
            }
        );
        assert_ne!(
            regex_constraint,
            Regex {
                regex: regex::Regex::new("abc").unwrap(),
                multiline: true,
                case_insensitive: false,
            }
        );
        assert_ne!(
            regex_constraint,
            Regex {
                regex: regex::Regex::new("abc").unwrap(),
                multiline: false,
                case_insensitive: true,
            }
        );
        assert_ne!(
            regex_constraint,
            Regex {
                regex: regex::Regex::new("def").unwrap(),
                multiline: true,
                case_insensitive: true,
            }
        );
    }
}
