// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

// TODO: This file is a placeholder. These things will eventually be moved to more sensible locations
//       instead of being clobbered together.

use crate::result::{invalid_schema, IonSchemaResult};
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::ValueWriter;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::ControlFlow;

/// For internal implementation of validation.
///
/// Should be implemented by TypeDefinition, TypeArgument, etc. and all constraints.
pub(crate) trait ValidateInternal {
    /// Validate a value.
    /// Panics if any unresolved type references are encountered. This crate must
    /// ensure that resolve() is called before calling validate.
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &'top IonSchemaElement,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>;
}

// TODO: fields/functions to support
//  - looking up references in the InvisibleSchemaStore
//  - any other state or needed for validation
//  - "validate" configuration options
pub(crate) struct ValidationContext {}

/// For internal implementation of serialization.
///
/// Implementations of `WriteAsIon` may delegate to this when possible.
pub(crate) trait WriteAsIsl<V: IslVersion>: Debug {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        let (major, minor) = V::MAJOR_MINOR;
        invalid_schema!("{self:?} is not supported in Ion Schema Language {major}.{minor}")
    }
}

/// Contains context for writing to ISL.
///
/// For now, this includes only the version as a type parameter.
/// In the future, it may include things such as:
///  - options to allow lossy conversion between ISL versions
///  - stylistic choices for writing as Ion
#[derive(Copy, Clone, Debug)]
pub(crate) struct WriteContext<V> {
    version: PhantomData<V>,
    /// Indicates whether ranges may be "minimized" into single values.
    /// E.g.: writing `range::[1, 1]` as `1`.
    pub(crate) minimize_ranges: bool,
}
impl<V: IslVersion> WriteContext<V> {
    pub fn new() -> Self {
        WriteContext {
            version: PhantomData::<V>,
            minimize_ranges: true,
        }
    }
}
