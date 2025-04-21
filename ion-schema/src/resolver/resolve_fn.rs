// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

pub(crate) use super::*;
use crate::model::{SchemaDocument, SchemaItem, TypeReference};
use crate::result::{
    invalid_schema as ion_schema_error_invalid_schema, InvalidSchemaError,
    InvalidSchemaErrorCollector,
};
use crate::IonSchemaResult;
use std::collections::HashMap;
use std::sync::Arc;

// Private type aliases
type Map<K, V> = HashMap<K, V>;
type CoordinatesMap = Map<String, TypeCoordinates>;
type SchemaSlice = [(String, SchemaDocument)];
type ResolverResult<T> = Result<T, InvalidSchemaError>;

// Shim to change the return type of invalid_schema! from IonSchemaError to InvalidSchemaError
macro_rules! invalid_schema {
    ($($tt:tt)+) => {
        {
            let error: InvalidSchemaError = ion_schema_error_invalid_schema!($($tt)+).try_into().unwrap();
            error
        }
    };
}

/// Builds a lookup table for the type coordinates of all types declared in all schemas.
fn build_global_coordinates(schemas: &SchemaSlice) -> Map<String, CoordinatesMap> {
    schemas
        .iter()
        .enumerate()
        .map(|(schema_idx, (schema_id, _))| {
            // We ignore _resolution_result here because any errors are handled in build_locally_available_coordinates
            let (local_coordinates, _resolution_result) =
                build_locally_declared_coordinates(schema_idx, schemas);
            (schema_id.to_string(), local_coordinates)
        })
        .collect()
}

/// Builds a lookup table for the type coordinates of all locally declared types.
/// This function always provides a best-effort `CoordinatesMap`, as well as an `IonSchemaResult`
/// indicating whether any errors occurred.
fn build_locally_declared_coordinates(
    schema_idx: usize,
    schemas: &SchemaSlice,
) -> (CoordinatesMap, ResolverResult<()>) {
    let (schema_id, schema_doc) = &schemas[schema_idx];
    let mut errors = InvalidSchemaErrorCollector::default();
    let mut coordinates = CoordinatesMap::new();

    let indexed_schema_types = schema_doc.items().enumerate().filter_map(|(i, item)| {
        if let SchemaItem::Type(name, def) = item {
            Some((i, name, def))
        } else {
            None
        }
    });
    for (type_idx, type_name, type_def) in indexed_schema_types {
        let conflict =
            coordinates.insert(type_name.to_string(), TypeCoordinates(schema_idx, type_idx));
        if conflict.is_some() {
            errors.push_err(invalid_schema!(
                schema_id,
                type_def,
                "type '{type_name}' is defined multiple times"
            ));
        }
    }
    (coordinates, errors.into_result_with(()))
}

/// Builds a lookup table for the type coordinates of all types that are visible _within_ the schema
/// specified by [`schema_idx`]
fn build_locally_available_coordinates(
    global_coordinates: &Map<String, CoordinatesMap>,
    schema_idx: usize,
    schemas: &SchemaSlice,
) -> ResolverResult<CoordinatesMap> {
    let mut errors = InvalidSchemaErrorCollector::default();
    let (mut coordinates, result) = build_locally_declared_coordinates(schema_idx, schemas);
    errors.ok_or_push_err(result);
    let (schema_name, schema_doc) = &schemas[schema_idx];
    let Some(header) = schema_doc.header() else {
        return errors.into_result_with(coordinates);
    };

    // Track the sources of the imported coordinates, so that if there's a conflict between imports, we can show both
    let mut import_sources = HashMap::new();

    for import in header.imports() {
        let imported_schema_name = import.schema_id().to_string();
        let Some(imported_coordinates_map) = global_coordinates.get(&imported_schema_name) else {
            // This can only happen if people are manually constructing schemas because the
            // loader ensures that all imported schemas are present.
            errors.push_err(invalid_schema!(
                schema_name,
                import,
                "target schema not found for import: {import}"
            ));
            continue;
        };

        if let Some(type_name) = import.type_name() {
            // Importing a specific type

            let local_name = import.type_alias().unwrap_or(type_name);
            let Some(tc) = imported_coordinates_map.get(&type_name.to_string()) else {
                errors.push_err(invalid_schema!(
                    schema_name,
                    import,
                    "target type not found for import: {import}"
                ));
                continue;
            };
            let conflict = coordinates.insert(local_name.to_string(), *tc);
            match conflict.map(|conflict_tc| import_sources.get(&conflict_tc)) {
                None => {}
                Some(None) => errors.push_err(invalid_schema!(
                    schema_name,
                    import,
                    "conflict for type '{local_name}' declared locally and imported by {import}"
                )),
                Some(Some(first_import)) => errors.push_err(invalid_schema!(
                    schema_name,
                    import,
                    "conflict for type '{local_name}' imported by '{first_import}' and by {import}"
                )),
            }
            import_sources.insert(*tc, import);
        } else {
            // Importing a whole schema

            for (local_name, tc) in imported_coordinates_map.iter() {
                let conflict = coordinates.insert(local_name.to_string(), *tc);
                match conflict.map(|conflict_tc| import_sources.get(&conflict_tc)) {
                    None => {}
                    Some(None) => errors.push_err(invalid_schema!(schema_name, import, "conflict for type '{local_name}' declared locally and imported by {import}")),
                    Some(Some(first_import)) => errors.push_err(invalid_schema!(schema_name, import, "conflict for type '{local_name}' imported by '{first_import}' and by {import}")),
                }
                import_sources.insert(*tc, import);
            }
        }
    }

    errors.into_result_with(coordinates)
}

/// Attempts to resolve the type references in one or more [`SchemaDocument`]s, returning an
/// equivalent map containing [`ResolvedSchema`]s rather than [`SchemaDocument`]s.
pub fn resolve(
    schemas: impl IntoIterator<Item = (String, SchemaDocument)>,
) -> IonSchemaResult<impl Iterator<Item = (String, ResolvedSchema)>> {
    let mut schemas: Vec<_> = schemas.into_iter().collect();

    if schemas.is_empty() {
        return Ok(vec![].into_iter());
    }

    if cfg!(test) {
        // This is not necessary for correctness. It just ensures that the type coordinates are stable
        // so that we can write tests for them.
        schemas.sort_by(|(name_a, _), (name_b, _)| name_a.cmp(name_b));
    }

    let schema_idx_by_name: HashMap<_, _> = schemas
        .iter()
        .enumerate()
        .map(|(idx, (name, _))| (name.clone(), idx))
        .collect();

    let global_type_coordinates = build_global_coordinates(schemas.as_slice());

    let mut type_resolution_errors = InvalidSchemaErrorCollector::default();

    // Create the locally available coordinate maps so that we can iterate them in parallel with the
    // schema definitions. This avoids some issues with borrowing schemas in the main loop, since
    // the main loop needs to borrow each schema mutably.
    let locally_available_coordinates: Vec<_> = schemas
        .iter()
        .enumerate()
        .map(|(schema_idx, (schema_id, schema))| {
            build_locally_available_coordinates(&global_type_coordinates, schema_idx, &schemas)
        })
        .collect();

    // Main Loop: For each schema, populate the type coordinates for all the type references.
    for ((schema_idx, local_type_coordinates), (schema_name, schema)) in
        locally_available_coordinates
            .into_iter()
            .enumerate()
            .zip(schemas.iter_mut())
    {
        match local_type_coordinates {
            Ok(local_type_coordinates) => {
                schema.walk(&mut |tr: &mut TypeReference| match lookup_type_coordinates(
                    schema_name,
                    tr,
                    &local_type_coordinates,
                    &global_type_coordinates,
                ) {
                    Ok(tc) => tr.set_type_coordinates(Some(tc)),
                    Err(e) => type_resolution_errors.push_err(e),
                });
            }
            Err(e1) => {
                // Something is wrong with the locally available coordinates. We'll still try to check
                // the type references on a best-effort basis in order to provide a complete list of errors.
                type_resolution_errors.push_err(e1);
                let partial_local_coordinates = global_type_coordinates.get(schema_name).unwrap();
                schema.walk(&mut |tr: &mut TypeReference| {
                    if let Err(e2) = lookup_type_coordinates(
                        schema_name,
                        tr,
                        partial_local_coordinates,
                        &global_type_coordinates,
                    ) {
                        type_resolution_errors.push_err(e2);
                    }
                });
            }
        };
    }

    let schema_store = Arc::new(SchemaStore { schemas });

    let result: Vec<_> = schema_idx_by_name
        .into_iter()
        .map(|(name, index)| (name, ResolvedSchema::new(schema_store.clone(), index)))
        .collect();

    Ok(type_resolution_errors.into_result_with(result.into_iter())?)
}

fn lookup_type_coordinates(
    schema_name: &str,
    tr: &TypeReference,
    local_type_coordinates: &CoordinatesMap,
    global_type_coordinates: &Map<String, CoordinatesMap>,
) -> ResolverResult<TypeCoordinates> {
    let type_name = tr.type_name();
    let coordinates = if let Some(imported_schema_id) = tr.schema_id() {
        let imported_coordinates =
            global_type_coordinates
                .get(imported_schema_id)
                .ok_or_else(|| {
                    invalid_schema!(schema_name, tr, "target schema not found for inline import {{ id:\"{imported_schema_id}\", type:'{type_name}' }}")
                })?;
        imported_coordinates.get(type_name).ok_or_else(|| {
            invalid_schema!(schema_name, tr, "target type not found for inline import {{ id:\"{imported_schema_id}\", type:'{type_name}' }}")
        })?
    } else {
        local_type_coordinates.get(type_name).ok_or_else(|| {
            invalid_schema!(
                schema_name,
                tr,
                "target type not found for type reference '{type_name}'"
            )
        })?
    };
    Ok(*coordinates)
}

/// Un-resolves the schemas and returns ownership of the underlying [`SchemaDocument`]s to the caller,
/// in a map of `schema_id` to `SchemaDocument`.
///
/// If there are any un-dropped `ResolvedSchema` or `ResolvedType` that depend on the underlying
/// `SchemaDocument`s that are _not_ passed into this function, the function will return `None`.
pub fn unresolve(
    schemas: impl IntoIterator<Item = (String, ResolvedSchema)>,
) -> Option<impl Iterator<Item = (String, SchemaDocument)>> {
    let schemas: Vec<_> = schemas.into_iter().collect();
    if schemas.is_empty() {
        return Some(vec![].into_iter());
    }

    let (_, any_resolved_schema) = schemas.first().unwrap();
    let schema_store = any_resolved_schema.schema_store();

    drop(schemas);

    let unresolved_schemas: Vec<_> = Arc::into_inner(schema_store)?
        .schemas
        .into_iter()
        .map(|(name, mut schema)| {
            schema.walk(&mut |tr: &mut TypeReference| tr.set_type_coordinates(None));
            (name, schema)
        })
        .collect();
    Some(unresolved_schemas.into_iter())
}

#[cfg(test)]
mod tests {
    use crate::loader::{ReadFromIsl, ReaderContext};
    use crate::model::constraints::FieldsContent;
    use crate::model::*;
    use crate::resolver::{resolve, unresolve, ResolvedSchema, TypeCoordinates, TypeRefWalker};

    use crate::{Versioned, ISL_1_0, ISL_2_0};
    use ion_rs::Element;
    use std::collections::HashMap;

    #[test]
    fn test_resolve_local_reference() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("foo", TypeDefinition::builder().build())
                .type_definition(
                    "bar",
                    TypeDefinition::builder().type_constraint("foo").build(),
                )
                .build(),
        );

        let resolved = resolve(schemas).unwrap();
        resolved.into_iter().for_each(|(_, schema)| {
            schema
                .as_schema_document()
                .clone()
                .walk(&mut |tr: &mut TypeReference| {
                    assert_eq!(Some(TypeCoordinates(0, 0)), tr.type_coordinates())
                })
        });
    }

    #[test]
    fn test_resolve_nested_reference() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("foo", TypeDefinition::builder().build())
                .type_definition(
                    "bar",
                    TypeDefinition::builder()
                        .element(
                            TypeDefinition::builder()
                                .one_of([TypeDefinition::builder()
                                    .fields(FieldsContent::Open, [("field", "foo".required())])
                                    .build()])
                                .build(),
                        )
                        .build(),
                )
                .build(),
        );

        let resolved = resolve(schemas).unwrap();
        resolved.into_iter().for_each(|(_, schema)| {
            schema
                .as_schema_document()
                .clone()
                .walk(&mut |tr: &mut TypeReference| {
                    assert_eq!(Some(TypeCoordinates(0, 0)), tr.type_coordinates())
                })
        });
    }

    #[test]
    fn test_resolve_self_reference() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("foo", TypeDefinition::builder().element("foo").build())
                .build(),
        );

        let resolved = resolve(schemas).unwrap();
        resolved.into_iter().for_each(|(_, schema)| {
            schema
                .as_schema_document()
                .clone()
                .walk(&mut |tr: &mut TypeReference| {
                    assert_eq!(Some(TypeCoordinates(0, 0)), tr.type_coordinates())
                })
        });
    }

    #[test]
    fn test_resolve_circular_reference() {
        let schema = SchemaDocument::builder::<ISL_2_0>()
            .type_definition("foo", TypeDefinition::builder().element("bar").build())
            .type_definition("bar", TypeDefinition::builder().element("foo").build())
            .build();

        let resolved_schema: ResolvedSchema = schema.try_into().unwrap();

        let foo = resolved_schema.get_type("foo").unwrap();
        foo.as_ref().clone().walk(&mut |tr: &mut TypeReference| {
            assert_eq!(Some(TypeCoordinates(0, 1)), tr.type_coordinates())
        });

        let bar = resolved_schema.get_type("bar").unwrap();
        bar.as_ref().clone().walk(&mut |tr: &mut TypeReference| {
            assert_eq!(Some(TypeCoordinates(0, 0)), tr.type_coordinates())
        });
    }

    #[test]
    fn test_resolve_inline_import_reference() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("foo", TypeDefinition::builder().build())
                .type_definition(
                    "bar",
                    TypeDefinition::builder()
                        .type_constraint(TypeReference::imported("bbb.isl", "bar"))
                        .build(),
                )
                .build(),
        );
        schemas.insert(
            "bbb.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("foo", TypeDefinition::builder().build())
                .type_definition("bar", TypeDefinition::builder().build())
                .build(),
        );

        let resolved = resolve(schemas).unwrap();
        resolved.into_iter().for_each(|(_, schema)| {
            schema
                .as_schema_document()
                .clone()
                .walk(&mut |tr: &mut TypeReference| {
                    assert_eq!(Some(TypeCoordinates(1, 1)), tr.type_coordinates())
                })
        });
    }

    #[test]
    fn test_resolve_schema_imported_reference() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .header(SchemaHeader::builder().import("bbb.isl").build())
                .type_definition("foo", TypeDefinition::builder().build())
                .type_definition(
                    "bar",
                    TypeDefinition::builder().type_constraint("baz").build(),
                )
                .build(),
        );
        schemas.insert(
            "bbb.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("baz", TypeDefinition::builder().build())
                .type_definition("bat", TypeDefinition::builder().build())
                .build(),
        );

        let resolved = resolve(schemas).unwrap();
        resolved.into_iter().for_each(|(_, schema)| {
            schema
                .as_schema_document()
                .clone()
                .walk(&mut |tr: &mut TypeReference| {
                    assert_eq!(Some(TypeCoordinates(1, 0)), tr.type_coordinates())
                })
        });
    }

    #[test]
    fn test_resolve_schema_type_imported_reference() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .header(SchemaHeader::builder().import(("bbb.isl", "baz")).build())
                .type_definition("foo", TypeDefinition::builder().build())
                .type_definition(
                    "bar",
                    TypeDefinition::builder().type_constraint("baz").build(),
                )
                .build(),
        );
        schemas.insert(
            "bbb.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("baz", TypeDefinition::builder().build())
                .type_definition("bat", TypeDefinition::builder().build())
                .build(),
        );

        let resolved = resolve(schemas).unwrap();
        resolved.into_iter().for_each(|(_, schema)| {
            schema
                .as_schema_document()
                .clone()
                .walk(&mut |tr: &mut TypeReference| {
                    assert_eq!(Some(TypeCoordinates(1, 0)), tr.type_coordinates())
                })
        });
    }

    #[test]
    fn test_resolve_schema_type_alias_imported_reference() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .header(
                    SchemaHeader::builder()
                        .import(("bbb.isl", "baz", "pizza"))
                        .build(),
                )
                .type_definition("foo", TypeDefinition::builder().build())
                .type_definition(
                    "bar",
                    TypeDefinition::builder().type_constraint("pizza").build(),
                )
                .build(),
        );
        schemas.insert(
            "bbb.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("baz", TypeDefinition::builder().build())
                .type_definition("bat", TypeDefinition::builder().build())
                .build(),
        );

        let resolved = resolve(schemas).unwrap();
        resolved.into_iter().for_each(|(_, schema)| {
            schema
                .as_schema_document()
                .clone()
                .walk(&mut |tr: &mut TypeReference| {
                    assert_eq!(Some(TypeCoordinates(1, 0)), tr.type_coordinates())
                })
        });
    }

    #[test]
    fn test_unresolve() {
        let mut schemas = HashMap::new();
        schemas.insert("foo.isl".to_string(), foo_schema());
        schemas.insert("bar.isl".to_string(), bar_schema());
        let original = schemas.clone();
        let unresolved = unresolve(resolve(schemas).unwrap()).unwrap().collect();
        assert_eq!(original, unresolved);

        // Check that all type coordinates have been cleared
        unresolved.into_iter().for_each(|(_, mut schema)| {
            schema.walk(&mut |tr: &mut TypeReference| assert!(tr.type_coordinates().is_none()))
        });
    }

    #[test]
    fn test_unresolve_does_not_return_schemas_until_all_are_unresolved() {
        let mut schemas = HashMap::new();
        schemas.insert("foo.isl".to_string(), foo_schema());
        schemas.insert("bar.isl".to_string(), bar_schema());

        let original = schemas.clone();
        let resolved: HashMap<_, _> = resolve(schemas).unwrap().collect();

        let resolved_clone = resolved.clone();

        let unresolved = unresolve(resolved);
        assert!(unresolved.is_none());

        // Check that type coordinates have not been cleared
        resolved_clone.iter().for_each(|(_, schema)| {
            schema
                .as_schema_document()
                .clone()
                .walk(&mut |tr: &mut TypeReference| assert!(tr.type_coordinates().is_some()))
        });

        let unresolved = unresolve(resolved_clone).unwrap().collect();
        assert_eq!(original, unresolved);
    }

    macro_rules! assert_resolver_err {
        ($schemas:expr $(, $fun:expr)?) => {
            match resolve($schemas) {
                Ok(_) => panic!("expected a type resolver error"),
                Err(e) => {
                    println!("{e}");
                    $($fun(e);)?
                }
            }
        };
    }

    #[test]
    fn duplicate_type_name_should_cause_err() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("foo", TypeDefinition::builder().build())
                .type_definition("foo", TypeDefinition::builder().build())
                .build(),
        );
        assert_resolver_err!(schemas)
    }

    #[test]
    fn missing_import_should_cause_err() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .header(SchemaHeader::builder().import("foo.isl").build())
                .build(),
        );
        assert_resolver_err!(schemas)
    }

    #[test]
    fn missing_import_type_should_cause_err() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .header(
                    SchemaHeader::builder()
                        .import(("foo.isl", "type_a"))
                        .build(),
                )
                .build(),
        );
        schemas.insert(
            "foo.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("type_b", TypeDefinition::builder().build())
                .build(),
        );
        assert_resolver_err!(schemas)
    }

    #[test]
    fn duplicate_type_alias_should_cause_err() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .header(
                    SchemaHeader::builder()
                        .import(("foo.isl", "type_a", "alias_a"))
                        .import(("foo.isl", "type_b", "alias_a"))
                        .build(),
                )
                .build(),
        );
        schemas.insert(
            "foo.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("type_a", TypeDefinition::builder().build())
                .type_definition("type_b", TypeDefinition::builder().build())
                .build(),
        );
        assert_resolver_err!(schemas)
    }

    #[test]
    fn import_conflict_with_local_type_should_cause_err() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "aaa.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .header(
                    SchemaHeader::builder()
                        .import(("foo.isl", "type_a"))
                        .build(),
                )
                .type_definition("type_a", TypeDefinition::builder().build())
                .build(),
        );
        schemas.insert(
            "foo.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                .type_definition("type_a", TypeDefinition::builder().build())
                .build(),
        );
        assert_resolver_err!(schemas)
    }

    #[test]
    fn multiple_errs_can_be_reported() {
        let schema_ion = "\
$ion_schema_2_0

type::{
  name: foo,
  type: bar,
}
type::{
  name: foo,
  type: baz,
}
        ";

        let schema_items = Element::read_all(schema_ion).unwrap();

        let ctx = ReaderContext::<ISL_2_0>::new();
        let mut schemas = HashMap::new();
        schemas.insert(
            "my_schema.isl".to_string(),
            SchemaDocument::builder::<ISL_2_0>()
                // Unresolvable type reference `bar`
                .type_definition(
                    "foo",
                    Versioned::new(
                        TypeDefinition::try_read(schema_items.get(1).unwrap(), &ctx).unwrap(),
                    ),
                )
                // Redefinition of `foo`
                // Unresolvable type reference `baz`
                .type_definition(
                    "foo",
                    Versioned::new(
                        TypeDefinition::try_read(schema_items.get(2).unwrap(), &ctx).unwrap(),
                    ),
                )
                .build(),
        );
        assert_resolver_err!(schemas, |e| {
            let description = format!("{e}");
            let description: Vec<_> = description.lines().collect();
            assert_eq!(description.len(), 3);
            assert!(description[0].starts_with("my_schema.isl:5:9"));
            assert!(description[1].starts_with("my_schema.isl:7:1"));
            assert!(description[2].starts_with("my_schema.isl:9:9"));
        })
    }

    // Test Fixtures

    fn foo_schema() -> SchemaDocument {
        SchemaDocument::builder::<ISL_2_0>()
            .header(SchemaHeader::builder().import("bar.isl").build())
            .type_definition(
                "TypeA",
                TypeDefinition::builder()
                    .not("TypeB")
                    .type_constraint("TypeC")
                    .build(),
            )
            .type_definition(
                "TypeB",
                TypeDefinition::builder().container_length(1..10).build(),
            )
            .type_definition(
                "TypeC",
                TypeDefinition::builder()
                    .any_of((
                        "TypeB",
                        TypeReference::imported("bar.isl", "TypeE"),
                        "TypeF",
                    ))
                    .build(),
            )
            .build()
    }

    fn bar_schema() -> SchemaDocument {
        SchemaDocument::builder::<ISL_1_0>()
            .header(
                SchemaHeader::builder()
                    .import(("foo.isl", "TypeB"))
                    .import(("foo.isl", "TypeC", "AliasC"))
                    .import(("foo.isl", "TypeA", "TypeC"))
                    .build(),
            )
            .type_definition(
                "TypeD",
                TypeDefinition::builder()
                    .any_of(("TypeB", "AliasC", "TypeC"))
                    .build(),
            )
            .type_definition(
                "TypeE",
                TypeDefinition::builder()
                    .precision(1..10)
                    .scale(1..10)
                    .build(),
            )
            .type_definition(
                "TypeF",
                TypeDefinition::builder()
                    .not(TypeReference::imported("foo.isl", "TypeA"))
                    .build(),
            )
            .build()
    }
}
