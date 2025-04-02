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
/// This macro makes uses the "Incremental TT Muncher" pattern to create a cartesian product of ISL
/// versions and test case inputs. See https://veykril.github.io/tlborm/decl-macros/patterns/tt-muncher.html
/// for details about this pattern.
///
/// Steps/Cases:
///   1. The entry point—this sets up the enclosing module, and matches all the "case groups" (a
///      set of ISL versions and a set of test cases for those versions). Each matched case group
///      is handed off to the version muncher (step 2).
///   2. For a given case group, the first ISL version _and_ all the test case inputs are passed to
///      step 3. The remaining ISL versions _and_ all test case inputs are passed to a recursive
///      invocation of the version muncher. If there are no more ISL versions left to match, then it
///      will match step 2B, ending the recursion. In essence, this is loop over the ISL versions,
///      implemented using tail recursion (the opposite of tail-call loop optimization).
///   3. For a given ISL version, emits a `#[test]` function for each test-case input.
///
macro_rules! test_harness {
    // STEP 1
    (use $test:ident; $( $($versions:ident),+ { $(#[case::$case_name:ident($($args:expr),+)])+ } )+) => {
        #[cfg(test)]
        paste::paste!(
            mod [<test_ $test>] {
                use super::*;
                use paste::paste;
                use $test as the_test_fn;

                $(
                test_harness!(@munch_version $($versions),+ { $(#[case::$case_name($($args),+)])+} );
                )+
            }
        );
    };
    // STEP 2A
    (@munch_version $version:ident $(,$other_versions:ident)* { $(#[case::$case_name:ident($($args:expr),+)])+ }) => {
        test_harness!(@emit_fn $($case_name, $version, $($args),+)+);
        test_harness!(@munch_version $($other_versions),* { $(#[case::$case_name($($args),+)])+} );
    };
    // STEP 2B
    (@munch_version { $(#[case::$case_name:ident($($args:expr),+)])+ }) => {
        // No more versions; end the version munching
    };
    // STEP 3
    (@emit_fn $($case_name:ident, $version:ident, $($args:expr),+)+) => {
        paste!(
            $(
            #[test]
            fn [< $case_name _ $version:lower >]() {
                the_test_fn($($args,)+ std::marker::PhantomData::<$version>::default())
            }
            )+
        );
    };
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
    T: crate::loader::ReadFromIsl<V> + PartialEq + Debug,
>(
    ion: &str,
    expected: Result<T, ()>,
    version: std::marker::PhantomData<V>,
) {
    use crate::loader::ReaderContext;
    use ion_rs::Element;

    let element = Element::read_one(ion).unwrap();
    let load_ctx = ReaderContext::<V>::new();
    let result = T::try_read(&element, &load_ctx);

    if let Ok(expected_constraint) = expected {
        let actual_constraint = result.unwrap();
        assert_eq!(expected_constraint, actual_constraint);
    } else {
        assert!(result.is_err())
    }
}
