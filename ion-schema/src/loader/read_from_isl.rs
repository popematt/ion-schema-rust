// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::result::{invalid_schema_2, InvalidSchemaError};
use crate::{IslVersion, ISL_2_0};
use ion_rs::Element;
use std::marker::PhantomData;

#[derive(Debug, Copy, Clone)]
pub(crate) struct ReaderContext<V> {
    pub(super) version: PhantomData<V>,
    pub is_top_level: bool,
}

impl<V> ReaderContext<V> {
    pub fn new() -> Self {
        Self {
            version: PhantomData::<V>,
            is_top_level: true,
        }
    }

    pub(super) fn with_version<NewVersion: IslVersion>(&self) -> ReaderContext<NewVersion> {
        ReaderContext {
            version: PhantomData,
            is_top_level: self.is_top_level,
        }
    }
}

/// Type alias for the return type of ReadFromIsl
pub(crate) type ReadResult<T> = Result<T, InvalidSchemaError>;
/// Type alias for a result that can return partial success.
pub(crate) type ReadResultWithPartialSuccess<T> = Result<T, (Option<T>, InvalidSchemaError)>;

/// For internal implementation of reading Ion Schema Language.
///
/// Items may implement [ReadFromIslWithPartialSuccess] if it is possible to return a partial result.
pub(crate) trait ReadFromIsl<V: IslVersion>: Sized {
    fn try_read(ion: &Element, ctx: &ReaderContext<V>) -> ReadResult<Self>;
}

/// For internal implementation of reading Ion Schema Language.
///
/// This trait has a blanket implementation for anything that implements [ReadFromIsl].
/// Items may implement [ReadFromIslWithPartialSuccess] if it is possible to return a partial result.
/// "Partial Success" ultimately gets reported to the user as an Error, but returning a partially
/// successful result allows additional inspection of the data, which may enable more complete error
/// messages.
pub(crate) trait ReadFromIslWithPartialSuccess<V: IslVersion>: Sized {
    fn try_read_with_partial_success(
        ion: &Element,
        ctx: &ReaderContext<V>,
    ) -> ReadResultWithPartialSuccess<Self>;
}
impl<V: IslVersion, T: ReadFromIsl<V>> ReadFromIslWithPartialSuccess<V> for T {
    fn try_read_with_partial_success(
        ion: &Element,
        ctx: &ReaderContext<V>,
    ) -> ReadResultWithPartialSuccess<Self> {
        Self::try_read(ion, ctx).map_err(|e| (None, e))
    }
}

/// Macro for trivial implementations that can use `expect_*` functions on [`Element`].
macro_rules! read_from_isl {
    ($t:ty, $func:ident, $ion_type_name:literal) => {
        impl<V: IslVersion> ReadFromIsl<V> for $t {
            fn try_read(ion: &Element, ctx: &ReaderContext<V>) -> ReadResult<Self> {
                match ion.$func() {
                    Ok(value) => Ok(value),
                    Err(_) => invalid_schema_2!(ion, "value must be a {}", $ion_type_name),
                }
            }
        }
    };
}

read_from_isl!(usize, expect_usize, "int");
read_from_isl!(i64, expect_i64, "int");
read_from_isl!(ion_rs::Decimal, expect_decimal, "decimal");
read_from_isl!(ion_rs::Timestamp, expect_timestamp, "timestamp");
