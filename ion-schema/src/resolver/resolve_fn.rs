// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

pub(crate) use super::*;
use crate::model::{SchemaDocument, TypeReference};
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, unresolvable_schema_error_raw, IonSchemaResult,
};
use std::collections::HashMap;
use std::sync::Arc;

// Private type aliases
type Map<K, V> = HashMap<K, V>;
type CoordinatesMap<'a> = Map<&'a str, TypeCoordinates>;
type SchemaSlice = [(String, SchemaDocument)];

/// Builds a lookup table for the type coordinates of all types declared in all schemas.
fn build_global_coordinates(schemas: &SchemaSlice) -> Map<&str, CoordinatesMap> {
    schemas
        .iter()
        .enumerate()
        .map(|(schema_idx, (schema_id, _))| {
            (
                schema_id.as_str(),
                build_locally_declared_coordinates(schema_idx, schemas),
            )
        })
        .collect()
}

/// Builds a lookup table for the type coordinates of all locally declared types
fn build_locally_declared_coordinates(
    schema_idx: usize,
    schemas: &SchemaSlice,
) -> Map<&str, TypeCoordinates> {
    let (_, schema_doc) = &schemas[schema_idx];
    schema_doc
        .indexed_type_names()
        .map(|(type_idx, type_name)| (type_name.as_str(), TypeCoordinates(schema_idx, type_idx)))
        .collect()
}

/// Builds a lookup table for the type coordinates of all types that are visible _within_ the schema
/// specified by [`schema_idx`]
fn build_locally_available_coordinates<'a>(
    global_coordinates: &'a Map<&'a str, CoordinatesMap<'a>>,
    schema_idx: usize,
    schemas: &'a SchemaSlice,
) -> IonSchemaResult<Map<&'a str, TypeCoordinates>> {
    let mut coordinates = build_locally_declared_coordinates(schema_idx, schemas);
    let (schema_name, schema_doc) = &schemas[schema_idx];
    let Some(header) = schema_doc.header() else {
        return Ok(coordinates);
    };

    for import in header.imports() {
        let imported_schema_name = import.schema_id();
        if let Some(type_name) = import.type_name() {
            let tc = global_coordinates
                .get(&imported_schema_name)
                .ok_or_else(|| {
                    unresolvable_schema_error_raw(format!(
                        "Cannot resolve schema '{imported_schema_name}'. Was it loaded?",
                    ))
                })?
                .get(type_name)
                .ok_or_else(|| {
                    invalid_schema_error_raw(format!(
                        "Type '{type_name}' in '{imported_schema_name}' does not exist."
                    ))
                })?;
            let local_name = if let Some(alias) = import.type_alias() {
                alias
            } else {
                type_name
            };
            if coordinates.insert(local_name, *tc).is_some() {
                invalid_schema_error(format!("name conflict for type '{local_name}' imported from '{imported_schema_name}' in schema '{schema_name}'"))?;
            }
        } else {
            let imported_types = global_coordinates
                .get(&imported_schema_name)
                // This can only happen if people are manually constructing schemas because the
                // loader ensures that all imported schemas are present.
                .ok_or_else(|| {
                    unresolvable_schema_error_raw(format!(
                        "Cannot resolve schema '{imported_schema_name}'. Was it loaded?"
                    ))
                })?;

            for (local_name, tc) in imported_types.iter() {
                if let Some(name_conflict) = coordinates.insert(local_name, *tc) {
                    let (conflict_schema_name, _) = &schemas[name_conflict.0];
                    invalid_schema_error(format!("name conflict in schema '{schema_name}' for type '{local_name}' declared in '{imported_schema_name}' and '{conflict_schema_name}'"))?;
                }
            }
        }
    }

    Ok(coordinates)
}

/// Attempts to resolve the type references in one or more [`SchemaDocument`]s, returning an
/// equivalent map containing [`ResolvedSchema`]s rather than [`SchemaDocument`]s.
pub fn resolve(
    schemas: HashMap<String, SchemaDocument>,
) -> IonSchemaResult<HashMap<String, ResolvedSchema>> {
    if schemas.is_empty() {
        return Ok(HashMap::new());
    }

    let mut schemas: Vec<_> = schemas.into_iter().collect();

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

    let mut type_resolution_errors = vec![];

    for (schema_idx, (schema_name, schema)) in schemas.iter().enumerate() {
        let local_type_coordinates =
            build_locally_available_coordinates(&global_type_coordinates, schema_idx, &schemas)?;

        schema.walk(&mut |tr: &TypeReference| {
            let type_name = tr.type_name();
            let type_ref_coordinates = if let Some(imported_schema_id) = tr.schema_id() {
                global_type_coordinates
                    .get(imported_schema_id)
                    .ok_or_else(|| {
                        unresolvable_schema_error_raw(format!(
                            "Cannot resolve schema '{imported_schema_id}'. Was it loaded?"
                        ))
                    })
                    .and_then(|ok| {
                        ok.get(type_name).ok_or_else(|| {
                            invalid_schema_error_raw(format!(
                                "No type '{type_name}' exists in '{imported_schema_id}'."
                            ))
                        })
                    })
                    .copied()
            } else {
                local_type_coordinates
                    .get(type_name)
                    .ok_or_else(|| {
                        unresolvable_schema_error_raw(format!(
                            "No type '{type_name}' exists in '{schema_name}'"
                        ))
                    })
                    .copied()
            };
            match type_ref_coordinates {
                Ok(tc) => {
                    // SAFETY: This is safe because
                    //  * All the data is fully owned within this function while this happens
                    //  * We know there is no concurrent access to this
                    //
                    // we know that there is no concurrent access to the type references.
                    //
                    // This is necessary because
                    //  * We can't just use `&mut TypeReference` because we run into conflicting borrows
                    //    because the loop would have to iterate over mutable references, but the global
                    //    and local coordinate maps require an immutable borrow at the same time in order
                    //    to borrow the schema and type names.
                    //  * The solution is interior mutability, but we don't want type references to use
                    //    Cell because then they are not Sync, and we don't want to use Mutex because
                    //    TypeReferences are in the hot path for validation.
                    //  * Thankfully, we don't need the TypeReferences to have interior mutability all
                    //    the time. It's only necessary for resolving (and un-resolving) the type references.
                    //  * So instead, we just have a limited scope for treating this as immutable.
                    //
                    // We could also work around this by creating a "WhileBeingResolvedSchema" that
                    // uses `Cell` to hold `TypeDefinition`s, but that's a lot of boilerplate code.
                    unsafe { as_mutable(tr).resolve_type_coordinates(Some(tc)) }
                }
                Err(e) => type_resolution_errors.push(e),
            }
        });
    }

    if !type_resolution_errors.is_empty() {
        // TODO: Report multiple errors
        IonSchemaResult::Err(type_resolution_errors.into_iter().next().unwrap())?;
    }

    let schemas = schemas.into_iter().collect();

    let schema_store = Arc::new(SchemaStore { schemas });

    Ok(schema_idx_by_name
        .into_iter()
        .map(|(name, index)| (name, ResolvedSchema::new(schema_store.clone(), index)))
        .collect())
}

/// Un-resolves the schemas and returns ownership of the underlying [`SchemaDocument`]s to the caller,
/// in a map of `schema_id` to `SchemaDocument`.
///
/// If there are any un-dropped `ResolvedSchema` or `ResolvedType` that depend on the underlying
/// `SchemaDocument`s that are _not_ passed into this function, the function will return `None`.
pub fn unresolve(
    schemas: HashMap<String, ResolvedSchema>,
) -> Option<HashMap<String, SchemaDocument>> {
    if schemas.is_empty() {
        return Some(HashMap::new());
    }

    let (_, any_resolved_schema) = schemas.iter().next().unwrap();
    let schema_store = any_resolved_schema.schema_store();

    drop(schemas);

    let unresolved_schemas = Arc::into_inner(schema_store)?
        .schemas
        .into_iter()
        .map(|(name, schema)| {
            schema.walk(&mut |tr: &TypeReference| {
                // SAFETY: See note in [`resolve`]. Justification is the same.
                unsafe { as_mutable(tr).resolve_type_coordinates(None) }
            });
            (name, schema)
        })
        .collect();
    Some(unresolved_schemas)
}

/// Cast an immutable reference to a mutable reference.
#[allow(clippy::mut_from_ref)]
unsafe fn as_mutable<T: ?Sized>(val: &T) -> &mut T {
    (val as *const T as *mut T).as_mut().unwrap_unchecked()
}

#[cfg(test)]
mod tests {
    use crate::model::constraints::FieldsContent;
    use crate::model::*;
    use crate::resolver::{resolve, unresolve, ResolvedSchema, TypeCoordinates, TypeRefWalker};
    use crate::{ISL_1_0, ISL_2_0};
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
        resolved.iter().for_each(|(_, schema)| {
            schema.walk(&mut |tr: &TypeReference| {
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
        resolved.iter().for_each(|(_, schema)| {
            schema.walk(&mut |tr: &TypeReference| {
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
        resolved.iter().for_each(|(_, schema)| {
            schema.walk(&mut |tr: &TypeReference| {
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
        foo.as_ref().walk(&mut |tr: &TypeReference| {
            assert_eq!(Some(TypeCoordinates(0, 1)), tr.type_coordinates())
        });

        let bar = resolved_schema.get_type("bar").unwrap();
        bar.as_ref().walk(&mut |tr: &TypeReference| {
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
        resolved.iter().for_each(|(_, schema)| {
            schema.walk(&mut |tr: &TypeReference| {
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
        resolved.iter().for_each(|(_, schema)| {
            schema.walk(&mut |tr: &TypeReference| {
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
        resolved.iter().for_each(|(_, schema)| {
            schema.walk(&mut |tr: &TypeReference| {
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
        resolved.iter().for_each(|(_, schema)| {
            schema.walk(&mut |tr: &TypeReference| {
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
        let unresolved = unresolve(resolve(schemas).unwrap()).unwrap();
        assert_eq!(original, unresolved);

        // Check that all type coordinates have been cleared
        unresolved.iter().for_each(|(_, schema)| {
            schema.walk(&mut |tr: &TypeReference| assert!(tr.type_coordinates().is_none()))
        });
    }

    #[test]
    fn test_unresolve_does_not_return_schemas_until_all_are_unresolved() {
        let mut schemas = HashMap::new();
        schemas.insert("foo.isl".to_string(), foo_schema());
        schemas.insert("bar.isl".to_string(), bar_schema());

        let original = schemas.clone();
        let resolved = resolve(schemas).unwrap();

        let resolved_clone: HashMap<String, ResolvedSchema> = resolved.clone();

        let unresolved = unresolve(resolved);
        assert!(unresolved.is_none());

        // Check that type coordinates have not been cleared
        resolved_clone.iter().for_each(|(_, schema)| {
            schema.walk(&mut |tr: &TypeReference| assert!(tr.type_coordinates().is_some()))
        });

        let unresolved = unresolve(resolved_clone).unwrap();
        assert_eq!(original, unresolved);
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
