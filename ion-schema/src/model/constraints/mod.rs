// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

mod annotations_v2_simple;
mod any_constraint;
mod timestamp_precision;
mod type_constraint;
mod utf8_byte_length;
mod valid_values;

pub use annotations_v2_simple::*;
pub use any_constraint::*;
pub use timestamp_precision::*;
pub use type_constraint::*;
pub use utf8_byte_length::*;
pub use valid_values::*;

/// Provides the constraint's name (ISL keyword) for a given constraint implementation.
pub(crate) trait ConstraintName {
    const CONSTRAINT_NAME: &'static str;
}
