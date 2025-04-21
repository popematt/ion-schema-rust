// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::result::{invalid_schema_2, InvalidSchemaError};
use crate::IslVersion;
use ion_rs::Element;
use std::marker::PhantomData;

#[derive(Debug, Copy, Clone)]
pub(crate) struct ReaderContext<V> {
    version: PhantomData<V>,
    pub is_top_level: bool,
}

impl<V: IslVersion> ReaderContext<V> {
    pub fn new() -> Self {
        Self {
            version: PhantomData::<V>,
            is_top_level: true,
        }
    }
}

/// Type alias for the return type of ReadFromIsl
pub(crate) type ReadResult<T> = Result<T, InvalidSchemaError>;

/// For internal implementation of reading Ion Schema Language.
pub(crate) trait ReadFromIsl<V: IslVersion>: Sized {
    fn try_read(ion: &Element, ctx: &ReaderContext<V>) -> ReadResult<Self>;
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
