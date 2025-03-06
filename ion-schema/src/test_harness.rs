// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use std::fmt::Debug;

/// Macro that can take rstest-like cases and inject necessary information to run for multiple
/// Ion Schema versions.
///
/// Example usage (marked `does_not_compile` because the macro is not pub, which means it will not work as a doc test):
/// ```does_not_compile
/// test_harness!(
///     write_as_isl:
///     ISL_1_0 {
///         #[case::foo_case("arg1", "arg2")]
///     }
///     ISL_1_0, ISL_2_0 {
///         #[case::bar_case("arg1", "arg2")]
///     }
/// )
/// ```
///
/// To be compatible with this test harness, the last parameter of the test function must be a
/// `PhantomData<V: IslVersion>`.
///
///
macro_rules! test_harness {
    // The entry point for the macro.
    ($test:ident: $($versions:ident),+ { $(#[case::$case_name:ident($($arg:expr),+)])+ }$(;$($tt:tt)*)?) => {
        #[cfg(test)]
        paste::paste!(
            mod [<test_ $test>] {
                use super::*;
                use paste::paste;
                use crate::test_harness::$test as the_test_fn;

                test_harness!(__goto__ __loop__: $($versions),* {$(#[case::$case_name($($arg),+)])+} $(;$($tt)*)?);
            }
        );
    };
    (__goto__ __loop__: $version:ident $(,$other_versions:ident)* { $(#[case::$case_name:ident($($args:expr),+)])+ }$(;$($tt:tt)*)?) => {
        test_harness!(__goto__ __emitfn__: $($case_name, $version, $($args),+)+);
        test_harness!(__goto__ __loop__: $($other_versions),* { $(#[case::$case_name($($args),+)])+ $(;$($tt)*)?} );
    };
    (__goto__ __loop__: { $(#[case::$case_name:ident($($args:expr),+)])+ }$(;$($tt:tt)*)?) => {
        test_harness!(__goto__ __loop__: $($($tt)*)?);
    };
    (__goto__ __loop__:) => {
        // No more groups
    };
    (__goto__ __emitfn__: $($case_name:ident, $version:ident, $($args:expr),+)+) => {
        paste!(
            $(
            #[test]
            fn [< $case_name _ $version:lower >]() {
                the_test_fn($($args,)+ std::marker::PhantomData::<$version>::default())
            }
            )+
        );
    }
}
pub(crate) use test_harness;

pub(crate) fn err<T>() -> Result<T, ()> {
    Err(())
}

/// Runs a test case for [`WriteAsIsl`](crate::internal_traits::WriteAsIsl).
pub(crate) fn write_as_isl<V: crate::IslVersion, T: crate::internal_traits::WriteAsIsl<V>>(
    expected_ion: Result<&str, ()>,
    constraint: T,
    version: std::marker::PhantomData<V>,
) {
    use crate::internal_traits::WriteContext;
    use ion_rs::v1_0::Text;
    use ion_rs::{Element, SequenceWriter, Writer};
    let buffer = Vec::new();
    let mut writer = Writer::new(Text, buffer).unwrap();
    let ctx = WriteContext::<V>::new();
    let result = constraint.write_as_isl(writer.value_writer(), &ctx);

    if let Ok(expected_ion) = expected_ion {
        let output = writer.close().unwrap();
        let actual_element = Element::read_one(output);
        let expected_element = Element::read_one(expected_ion);
        assert_eq!(expected_element, actual_element);
    } else {
        assert!(result.is_err())
    }
}

/// Runs a test case for [`ReadFromIsl`](crate::internal_traits::ReadFromIsl).
pub(crate) fn read_from_isl<
    V: crate::IslVersion,
    T: crate::internal_traits::ReadFromIsl<V> + PartialEq + Debug,
>(
    ion: &str,
    expected: Result<T, ()>,
    version: std::marker::PhantomData<V>,
) {
    use crate::internal_traits::LoaderContext;
    use ion_rs::Element;

    let element = Element::read_one(ion).unwrap();
    let load_ctx = LoaderContext::<V>::new();
    let result = T::try_read(&element, &load_ctx);

    if let Ok(expected_constraint) = expected {
        let actual_constraint = result.unwrap();
        assert_eq!(expected_constraint, actual_constraint);
    } else {
        assert!(result.is_err())
    }
}
