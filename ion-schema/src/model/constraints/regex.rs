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
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use regex::RegexBuilder;
use std::marker::PhantomData;
use std::ops::ControlFlow;

impl ConstraintName for Regex {
    const CONSTRAINT_NAME: &'static str = "regex";
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct RegexOptions<V: IslVersion> {
    // The `V` type parameter allows us to safely add new fields in new Ion Schema
    // versions without breaking compatibility.
    _version: PhantomData<V>,
    case_insensitive: bool,
    multiline: bool,
}
impl<V: IslVersion> RegexOptions<V> {
    fn new() -> Self {
        RegexOptions {
            _version: Default::default(),
            case_insensitive: false,
            multiline: false,
        }
    }

    fn case_insensitive(mut self, yes: bool) -> Self {
        self.case_insensitive = yes;
        self
    }

    fn multiline(mut self, yes: bool) -> Self {
        self.multiline = yes;
        self
    }
}
impl<V: IslVersion> From<()> for RegexOptions<V> {
    fn from(value: ()) -> Self {
        RegexOptions::new()
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
        self.regex.as_str() == other.regex.as_str()
            && self.case_insensitive == other.case_insensitive
            && self.multiline == other.multiline
    }
}

impl Regex {
    pub fn try_new<T: Into<String>, V: IslVersion>(
        multiline: bool,
        case_insensitive: bool,
        pattern: T,
    ) -> IonSchemaResult<Self> {
        // TODO: Clean up the different IslVersion representations
        let version = match V::MAJOR_MINOR {
            (1, 0) => crate::isl::IslVersion::V1_0,
            (2, 0) => crate::isl::IslVersion::V2_0,
            _ => unreachable!(),
        };

        let pattern = RegexConstraint::convert_to_pattern(pattern.into(), version)?;

        let regex = RegexBuilder::new(pattern.as_str())
            // TODO: See if this would fix https://github.com/amazon-ion/ion-rust/issues/399
            //.crlf(true)
            .case_insensitive(case_insensitive)
            .multi_line(multiline)
            .build()
            .map_err(|e| invalid_schema_error_raw(format!("Invalid regex: {pattern}")))?;

        Ok(Self {
            regex,
            multiline,
            case_insensitive,
        })
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn regex<T: Into<String>, O: Into<RegexOptions<V>>>(self, opts: O, pattern: T) -> Self {
        // TODO: Decide how to handle fallible builder methods, then get rid of the "unwrap".
        //       1. We could make builder methods return IonSchemaResult<Self>
        //       2. We could make the builder save Errs, and then surface them when calling `build()`.
        let opts = opts.into();
        let constraint =
            Regex::try_new::<_, V>(opts.multiline, opts.case_insensitive, pattern).unwrap();
        self.with_constraint(constraint.into())
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
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .regex((), "abc")
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Regex(
                Regex::try_new::<_, ISL_1_0>(false, false, "abc").unwrap()
            )]
        );

        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .regex(
                RegexOptions::new().case_insensitive(true).multiline(true),
                "abc",
            )
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Regex(
                Regex::try_new::<_, ISL_1_0>(true, true, "abc").unwrap()
            )]
        )
    }

    #[test]
    fn test_partial_eq() {
        let regex_constraint = Regex::try_new::<_, ISL_1_0>(true, true, "abc");

        assert_eq!(
            regex_constraint,
            Regex::try_new::<_, ISL_1_0>(true, true, "abc")
        );
        assert_ne!(
            regex_constraint,
            Regex::try_new::<_, ISL_1_0>(false, true, "abc")
        );
        assert_ne!(
            regex_constraint,
            Regex::try_new::<_, ISL_1_0>(true, false, "abc")
        );
        assert_ne!(
            regex_constraint,
            Regex::try_new::<_, ISL_1_0>(true, true, "def")
        );
    }
}
