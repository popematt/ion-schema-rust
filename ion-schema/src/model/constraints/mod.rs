// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

mod all_of;
mod annotations;
mod any_constraint;
mod any_of;
mod byte_length;
mod codepoint_length;
mod container_length;
mod contains;
mod element;
mod exponent;
mod field_names;
mod fields;
mod ieee754_float;
mod not;
mod one_of;
mod ordered_elements;
mod precision;
mod regex;
mod scale;
mod timestamp_offset;
mod timestamp_precision;
mod type_constraint;
mod utf8_byte_length;
mod valid_values;

use crate::IslVersion;
use ion_rs::Element;
use std::fmt::Debug;

use crate::loader::{ReadResult, ReaderContext};
pub use all_of::*;
pub use annotations::*;
pub use any_constraint::*;
pub use any_of::*;
pub use byte_length::*;
pub use codepoint_length::*;
pub use container_length::*;
pub use contains::*;
pub use element::*;
pub use exponent::*;
pub use field_names::*;
pub use fields::*;
pub use ieee754_float::*;
pub use not::*;
pub use one_of::*;
pub use ordered_elements::*;
pub use precision::*;
pub use regex::*;
pub use scale::*;
pub use timestamp_offset::*;
pub use timestamp_precision::*;
pub use type_constraint::*;
pub use utf8_byte_length::*;
pub use valid_values::*;

/// Provides the constraint's name (ISL keyword) for a given constraint implementation.
pub(crate) trait ConstraintName {
    const CONSTRAINT_NAME: &'static str;
}

pub(super) trait ReadConstraint<V: IslVersion>
where
    Self: Sized,
{
    fn read_constraint(ion: &Element, ctx: &ReaderContext<V>) -> ReadResult<Option<Self>> {
        // Default implementation indicates this is unsupported for a particular ISL version.
        Ok(None)
    }
}

#[cfg(test)]
/// Runs a test case for [`ReadConstraint`](crate::model::constraints::ReadConstraint).
fn test_read_constraint<V: IslVersion, T: ReadConstraint<V> + PartialEq + Debug>(
    ion: &str,
    expected: Result<Option<T>, ()>,
    version: std::marker::PhantomData<V>,
) {
    use crate::loader::ReaderContext;
    use ion_rs::Element;

    let element = Element::read_one(ion).unwrap();
    let load_ctx = ReaderContext::<V>::new();
    let result = T::read_constraint(&element, &load_ctx);

    if let Ok(expected_constraint) = expected {
        let actual_constraint = result.unwrap();
        assert_eq!(expected_constraint, actual_constraint);
    } else {
        assert!(result.is_err())
    }
}
