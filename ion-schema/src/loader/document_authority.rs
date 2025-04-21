// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::result::{IonSchemaError, IonSchemaResult};
use ion_rs::{Element, Sequence};
use paste::paste;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

///
///
/// The alternative is `IonSchemaResult<Option<IonResult<DocumentContent>>>` or
/// `IonSchemaResult<Option<impl Iterator<Item=IonResult<Element>>>>`.
pub enum DocumentAuthorityResponse {
    /// Indicates that the [`DocumentAuthority`] could not determine whether there was a schema
    /// with the given id.
    Unknown(IonSchemaError),
    /// Indicates that the [`DocumentAuthority`] does not contain a schema with the given id.
    None,
    /// Indicates that the [`DocumentAuthority`] contains a schema with the given id, but it could
    /// not be deserialized.
    SomeInvalid(IonSchemaError),
    /// The content for the requested schema id.
    SomeContent(DocumentContent),
}

/// The content of an Ion Schema document
pub(crate) struct DocumentContent {
    // TODO: Consider adding support for lazily-read documents
    elements: Vec<Element>,
}
impl DocumentContent {
    pub(crate) fn into_iter(self) -> impl Iterator<Item = Element> {
        self.elements.into_iter()
    }
}
impl From<Sequence> for DocumentContent {
    fn from(value: Sequence) -> Self {
        let elements = value.into_iter().collect();
        Self { elements }
    }
}
impl From<Vec<Element>> for DocumentContent {
    fn from(value: Vec<Element>) -> Self {
        Self { elements: value }
    }
}
impl<'a> From<&'a [Element]> for DocumentContent {
    fn from(value: &'a [Element]) -> Self {
        let elements = value.to_vec();
        Self { elements }
    }
}

/// Represents an entity that can resolve and provide schema definitions by their identifiers.
///
/// A `DocumentAuthority` is responsible for managing access to schema documents within a specific
/// namespace or domain. It acts as a resolver that can look up schema definitions based on their
/// unique identifiers.
pub trait DocumentAuthority {
    /// Gets the document content for the given schema id.
    ///
    /// Implementations should return:
    ///  * `Ok(Some(...))` when the authority contains a schema with the given ID
    ///  * `Ok(None)` when the authority contains no schema with the given ID
    ///  * `Err(...)` when there is a problem preventing the authority from determining
    ///    an answer (e.g. file access error or network outage)
    fn elements(&self, id: &str) -> IonSchemaResult<Option<DocumentContent>>;
}

impl DocumentAuthority for Path {
    /// Returns a vector of [`Element`]s based on given schema id
    fn elements(&self, id: &str) -> IonSchemaResult<Option<DocumentContent>> {
        let absolute_path = self.join(id);
        if !absolute_path.exists() {
            return Ok(None);
        }
        // if absolute_path exists for the given id then load schema with file contents
        let ion_content = fs::read(absolute_path)?;
        Ok(Some(Element::read_all(ion_content)?.into()))
    }
}

impl DocumentAuthority for HashMap<String, String> {
    fn elements(&self, id: &str) -> IonSchemaResult<Option<DocumentContent>> {
        if let Some(ion_content) = self.get(id) {
            Ok(Some(Element::read_all(ion_content)?.into()))
        } else {
            Ok(None)
        }
    }
}

impl DocumentAuthority for HashMap<String, Vec<Element>> {
    fn elements(&self, id: &str) -> IonSchemaResult<Option<DocumentContent>> {
        Ok(self.get(id).map(|elements| elements.as_slice().into()))
    }
}

impl DocumentAuthority for Box<dyn DocumentAuthority> {
    fn elements(&self, id: &str) -> IonSchemaResult<Option<DocumentContent>> {
        if let Some(content) = self.as_ref().elements(id)? {
            return Ok(Some(content));
        }
        Ok(None)
    }
}

/// Implements DocumentAuthority for various aggregates of other DocumentAuthorities.
macro_rules! composite_document_authority {
    ($type_:ty where $($tt:tt)+) => {
        impl $($tt)+ DocumentAuthority for $type_ {
            fn elements(&self, id: &str) -> IonSchemaResult<Option<DocumentContent>> {
                for da in self.iter() {
                    if let Some(content) = da.elements(id)? {
                        return Ok(Some(content));
                    }
                }
                Ok(None)
            }
        }
    };
    ($($i:tt),+) => {
        paste!(
            impl<$([<T $i>]),+> DocumentAuthority for ($([<T $i>],)+)
            where $([<T $i>]: DocumentAuthority,)+
            {
                fn elements(&self, id: &str) -> IonSchemaResult<Option<DocumentContent>> {
                    $(
                    if let Some(content) = self.$i.elements(id)? {
                        return Ok(Some(content));
                    }
                    )+
                    Ok(None)
                }
            }
        );
    };
}
composite_document_authority!(Vec<T> where <T: DocumentAuthority>);
composite_document_authority!(&[T] where <T: DocumentAuthority>);
composite_document_authority!([T; N] where <T: DocumentAuthority, const N: usize>);
composite_document_authority!(0);
composite_document_authority!(0, 1);
composite_document_authority!(0, 1, 2);
composite_document_authority!(0, 1, 2, 3);
composite_document_authority!(0, 1, 2, 3, 4);
composite_document_authority!(0, 1, 2, 3, 4, 5);
composite_document_authority!(0, 1, 2, 3, 4, 5, 6);
composite_document_authority!(0, 1, 2, 3, 4, 5, 6, 7);
composite_document_authority!(0, 1, 2, 3, 4, 5, 6, 7, 8);
composite_document_authority!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

#[cfg(test)]
mod tests {
    use crate::loader::{DocumentAuthority, DocumentContent};
    use crate::result::IonSchemaResult;
    use ion_rs::Element;

    impl DocumentAuthority for (String, String) {
        fn elements(&self, id: &str) -> IonSchemaResult<Option<DocumentContent>> {
            if self.0 == id {
                Ok(Some(
                    Element::read_all(&self.1)?
                        .iter()
                        .cloned()
                        .collect::<Vec<_>>()
                        .into(),
                ))
            } else {
                Ok(None)
            }
        }
    }

    // #[test]
    // fn test_authorities() {
    //     let authorities = (
    //         [],
    //
    //         );
    // }
}
