// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

mod annotations_v2_simple;
mod any_constraint;
mod type_constraint;

pub use annotations_v2_simple::*;
pub use any_constraint::*;
pub use type_constraint::*;

pub(crate) trait ConstraintName {
    const CONSTRAINT_NAME: &'static str;
}
