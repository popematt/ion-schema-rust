// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use std::fmt::Debug;
use std::slice;

/// An (internal only) bag implementation.
///
/// This naive implementation is backed by a sorted vec to have the same equality semantics as a bag.
/// However, this relies on having a total ordering that is consistent with `Eq`.
///
/// The current implementation sorts by the `Debug` representation of the value. This is not
/// particularly efficient, but until we have a better total ordering we can use, it will do.
///
/// Because of the sortedness invariant, adding or removing items one at a time is very inefficient,
/// and unsupported. Any modifications should be done by trivially converting to a `Vec`, applying
/// the modifications, and then converting back to a `Bag`.
///
/// TODO: Replace the vec with a HashBag or come up with a less hacky way to sort the type arguments.
#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Bag<T> {
    items: Vec<T>,
}

impl<T: PartialEq + Debug> Bag<T> {
    /// Constructs a new bag from the given items.
    pub fn from_items(items: impl IntoIterator<Item = T>) -> Self {
        let mut new = Self {
            items: items.into_iter().collect(),
        };
        new.sort_items();
        new
    }

    /// This is required to be performed after any add/remove operation in order to maintain the
    /// invariant that the items are sorted.
    fn sort_items(&mut self) {
        self.items.sort_by_key(|item| format!("{item:?}"));
    }
}

impl<T> Bag<T> {
    /// Returns the number of elements in the vector, also referred to
    /// as its 'length'.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Returns the _only_ element in the bag, or `None` if the bag has more than one element or is empty.
    pub fn single(&self) -> Option<&T> {
        if self.len() == 1 {
            Some(&self.items[0])
        } else {
            None
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.items.iter()
    }
}

impl<T> IntoIterator for Bag<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Bag<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl<T> FromIterator<T> for Bag<T>
where
    T: PartialEq + Debug,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut new = Self {
            items: iter.into_iter().collect(),
        };
        new.sort_items();
        new
    }
}

impl<T: PartialEq + Debug> From<Vec<T>> for Bag<T> {
    fn from(items: Vec<T>) -> Self {
        let mut new = Self { items };
        new.sort_items();
        new
    }
}

impl<T> From<Bag<T>> for Vec<T> {
    fn from(bag: Bag<T>) -> Self {
        bag.items
    }
}

macro_rules! bag {
    ($($tt:tt)*) => {
        crate::model::Bag::from(vec![$($tt)*])
    };
}
pub(crate) use bag;
