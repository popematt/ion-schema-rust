// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::loader::ReadResult;
use crate::result::invalid_schema_2;
use ion_rs::{Decimal, Sequence, Struct, Symbol};
use ion_rs::{Element, Value};
use num_traits::ToPrimitive;
use paste::paste;

macro_rules! require {
    ($method:ident -> $return_type:ty ;) => {
        paste!(
            fn [< require_ $method >] (&self, location: &str) -> ReadResult<&$return_type>;
        );
    };
    ($method:ident -> $return_type:ty {}) => {
        paste!(
            fn [< require_ $method >] (&self, location: &str) -> ReadResult<&$return_type> {
                match self.[< as_ $method >]() {
                    Some(value) => Ok(value),
                    None => invalid_schema_2!(self, "{location} must be a {}", stringify!($method)),
                }
            }
        );
    };
}

/// Trait for adding extensions to [`Element`] that are useful for implementing Ion Schema.
pub(crate) trait ElementExtensions {
    /// Returns some `usize` if this `Element` is an Ion Int _and_ it can be represented as (fits in) a `usize`.
    /// Returns `None` if `self` is not an Ion Int, or self is null.int, or self is out of bounds for `usize`.
    fn as_usize(&self) -> Option<usize>;
    /// Returns some `u64` if this `Element` is an Ion Int _and_ it can be represented as (fits in) a `u64`.
    /// Returns `None` if `self` is not an Ion Int, or self is null.int, or self is out of bounds for `u64`.
    fn as_u64(&self) -> Option<u64>;
    /// Returns some [`Decimal`] if this `Element` is any Ion number type (`int`, `decimal`, or `float`)
    /// _and_ it can be represented as (fits in) a `Decimal`. Returns `None` if `self` is not one
    /// of the Ion number types or not a finite value.
    fn any_number_as_decimal(&self) -> Option<Decimal>;
    /// Get up to one annotation from this [`Element`]. If this [`Element`] has more than one
    /// annotation, or if the only annotation has unknown text, returns [`Err`].
    fn one_optional_annotation(&self) -> ReadResult<Option<&str>>;
    /// If this [`Element`] is an Ion symbol with known text, returns [`Some`] with that text.
    /// Otherwise, returns [`None`].
    fn as_symbol_text(&self) -> Option<&str>;

    /// Gets the value for the given field name, returning Err if there is more than one value with
    /// that field name.
    fn get_optional_field(&self, location: &str, field_name: &str) -> ReadResult<Option<&Element>>;
    /// Gets the value for the given field name, returning Err if there are no values with that
    /// field name or if there is more than one value with that field name.
    fn get_required_field(&self, location: &str, field_name: &str) -> ReadResult<&Element>;

    fn require_struct(&self, location: &str) -> ReadResult<&Struct>;
    fn require_symbol(&self, location: &str) -> ReadResult<&Symbol>;
    fn require_list(&self, location: &str) -> ReadResult<&Sequence>;
    fn require_string(&self, location: &str) -> ReadResult<&str>;

    fn require_known_symbol(&self, location: &str) -> ReadResult<&str>;
}
impl ElementExtensions for Element {
    fn as_usize(&self) -> Option<usize> {
        match self.value() {
            Value::Int(i) => i.as_usize(),
            _ => None,
        }
    }
    fn as_u64(&self) -> Option<u64> {
        match self.value() {
            Value::Int(i) => i.as_i128()?.to_u64(),
            _ => None,
        }
    }
    fn any_number_as_decimal(&self) -> Option<Decimal> {
        match self.value() {
            Value::Int(i) => Some((*i).into()),
            Value::Float(f) => (*f).try_into().ok(),
            Value::Decimal(d) => Some(*d),
            _ => None,
        }
    }
    fn one_optional_annotation(&self) -> ReadResult<Option<&str>> {
        let mut annotations = self.annotations().iter();
        let Some(symbol) = annotations.next() else {
            return Ok(None);
        };
        let text = match symbol.text() {
            Some(t) => t,
            None => invalid_schema_2!(
                self,
                "cannot interpret value annotated with unknown symbol text"
            )?,
        };
        match annotations.next() {
            None => Ok(Some(text)),
            Some(_) => invalid_schema_2!(self, "Unexpected annotations: {self}"),
        }
    }
    fn as_symbol_text(&self) -> Option<&str> {
        self.as_symbol().and_then(|s| s.text())
    }

    fn get_optional_field(&self, location: &str, field_name: &str) -> ReadResult<Option<&Element>> {
        let mut iter = <Self as ElementExtensions>::require_struct(self, location)?.get_all(field_name);
        let first = iter.next();
        let second = iter.next();
        if second.is_some() {
            invalid_schema_2!(self, "Illegal repeated field '{field_name}' in {location}")
        } else {
            Ok(first)
        }
    }

    fn get_required_field(&self, location: &str, field_name: &str) -> ReadResult<&Element> {
        if let Some(element) = self.get_optional_field(location, field_name)? {
            Ok(element)
        } else {
            invalid_schema_2!(self, "Missing required field '{field_name}' in {location}")
        }
    }

    fn require_struct(&self, location: &str) -> ReadResult<&Struct> {
        match self.as_struct() {
            Some(value) => Ok(value),
            None => invalid_schema_2!( self , "{location} must be a struct"),
        }
    }

    fn require_symbol(&self, location: &str) -> ReadResult<&Symbol> {
        match self.as_symbol() {
            Some(value) => Ok(value),
            None => invalid_schema_2!( self , "{location} must be a symbol"),
        }
    }

    fn require_string(&self, location: &str) -> ReadResult<&str> {
        match self.as_string() {
            Some(value) => Ok(value),
            None => invalid_schema_2!( self , "{location} must be a string"),
        }
    }

    fn require_list(&self, location: &str) -> ReadResult<&Sequence> {
        match self.as_list() {
            Some(value) => Ok(value),
            None => invalid_schema_2!( self , "{location} must be a list"),
        }
    }

    fn require_known_symbol(&self, location: &str) -> ReadResult<&str> {
        match self.as_symbol() {
            Some(value) => {
                if let Some(text) = value.text() {
                    Ok(text)
                } else {
                    invalid_schema_2!(self, "{location} must be a symbol with known text")
                }
            }
            None => invalid_schema_2!(self, "{location} must be a symbol"),
        }
    }
}
