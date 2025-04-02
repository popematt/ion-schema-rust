// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::result::IonSchemaResult;
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

/// For internal implementation of reading Ion Schema Language.
pub(crate) trait ReadFromIsl<V: IslVersion>: Sized {
    fn try_read(ion: &Element, ctx: &ReaderContext<V>) -> IonSchemaResult<Self>;
}

/// Macro for trivial implementations that can use `expect_*` functions on [`Element`].
macro_rules! read_from_isl {
    ($t:ty, $func:ident) => {
        impl<V: IslVersion> ReadFromIsl<V> for $t {
            fn try_read(ion: &Element, ctx: &ReaderContext<V>) -> IonSchemaResult<Self> {
                let i = ion.$func()?;
                Ok(i)
            }
        }
    };
}

read_from_isl!(usize, expect_usize);
read_from_isl!(i64, expect_i64);
read_from_isl!(ion_rs::Decimal, expect_decimal);
read_from_isl!(ion_rs::Timestamp, expect_timestamp);
