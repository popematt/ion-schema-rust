// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{LoaderContext, ReadFromIsl, WriteAsIsl, WriteContext};
use crate::ion_extension::StructExtensions;
use crate::model::type_definition::TypeDefinition;
use crate::result::{invalid_schema_error, IonSchemaResult};
use crate::IslVersion;
use ion_rs::{Element, StructWriter, Value, ValueWriter};

/// References another type, by name, in Ion Schema.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeReference {
    schema_id: Option<String>,
    type_name: String,
    // Not exposed in public API.
    // Initially `None`, but changed to `Some(_)` when resolving the schemas
    // TODO: resolved: Cell<Option<...>>,
}

impl TypeReference {
    // TODO: Determine if this needs to be pub
    pub(crate) fn imported<A: Into<String>, B: Into<String>>(schema_id: A, type_name: B) -> Self {
        TypeReference {
            schema_id: Some(schema_id.into()),
            type_name: type_name.into(),
        }
    }

    pub fn schema_id(&self) -> Option<&str> {
        self.schema_id.as_deref()
    }

    pub fn type_name(&self) -> &str {
        self.type_name.as_str()
    }

    /// Returns an immutable borrow of the [TypeDefinition] to which this [TypeReference] refers.
    pub fn get(&self) -> Option<&TypeDefinition> {
        todo!()
    }

    // TODO: functions for setting the resolved type
}

impl<T: Into<String>> From<T> for TypeReference {
    fn from(value: T) -> Self {
        TypeReference {
            schema_id: None,
            type_name: value.into(),
        }
    }
}

impl<V: IslVersion> WriteAsIsl<V> for TypeReference {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        if let Some(schema_id) = self.schema_id() {
            let mut struct_writer = writer.struct_writer()?;
            struct_writer.field_writer("id").write(schema_id)?;
            struct_writer
                .field_writer("type")
                .write_symbol(self.type_name())?;
            struct_writer.close()?;
        } else {
            writer.write_symbol(self.type_name())?;
        }
        Ok(())
    }
}

impl<V: IslVersion> ReadFromIsl<V> for TypeReference {
    fn try_read(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Self> {
        match ion.value() {
            Value::Symbol(s) => Ok(s.expect_text()?.into()),
            Value::Struct(s) => {
                if s.fields().count() != 2 {
                    return invalid_schema_error(format!(
                        "Unexpected extra field(s) in inline import: {ion}"
                    ));
                }
                let schema_id = s.get_required("id")?.expect_text()?;
                let type_name = s.get_required("type")?.expect_text()?;
                Ok(TypeReference::imported(schema_id, type_name))
            }
            other => invalid_schema_error(format!(
                "TypeReference must be a symbol or struct; was: {ion}"
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::internal_traits::{LoaderContext, ReadFromIsl};
    use crate::model::type_reference::TypeReference;

    use crate::{ISL_1_0, ISL_2_0};
    use ion_rs::Element;
    use rstest::rstest;

    #[test]
    fn from_string() {
        let from_value = "foo";
        let expected = TypeReference {
            schema_id: None,
            type_name: "foo".to_string(),
        };
        assert_eq!(expected, from_value.into())
    }

    #[test]
    fn new_imported_type_reference() {
        let actual = TypeReference::imported("foo", "bar");
        let expected = TypeReference {
            schema_id: Some("foo".to_string()),
            type_name: "bar".to_string(),
        };
        assert_eq!(expected, actual)
    }

    #[rstest]
    #[case::type_name("foo", TypeReference { schema_id: None, type_name: "foo".to_string() })]
    #[case::inline_import("{id:\"foo.isl\",type:bar}", TypeReference { schema_id: Some("foo.isl".to_string()), type_name: "bar".to_string() } )]
    fn type_reference_try_read_ok(#[case] ion: &str, #[case] expected: TypeReference) {
        let expected = Ok(expected);
        let element = Element::read_one(ion).unwrap();
        let load_ctx = LoaderContext::<ISL_1_0>::new();
        let result = TypeReference::try_read(&element, &load_ctx);
        assert_eq!(result, expected);

        let load_ctx = LoaderContext::<ISL_2_0>::new();
        let result = TypeReference::try_read(&element, &load_ctx);
        assert_eq!(result, expected)
    }

    #[rstest]
    #[case::inline_import_must_have_id("{type:foo}")]
    #[case::inline_import_must_have_type("{id:\"foo\"}")]
    #[case::inline_import_must_not_have_other_fields("{id: \"abc\",type:def,as:ghi}")]
    fn type_constraint_try_read_err(#[case] ion: &str) {
        let element = Element::read_one(ion).unwrap();
        let load_ctx = LoaderContext::<ISL_1_0>::new();
        let result = TypeReference::try_read(&element, &load_ctx);
        assert!(result.is_err());

        let load_ctx = LoaderContext::<ISL_2_0>::new();
        let result = TypeReference::try_read(&element, &load_ctx);
        assert!(result.is_err())
    }
}
