use crate::import::Import;
use crate::system::TypeStore;
use crate::types::{TypeDefinition, TypeRef};
use std::rc::Rc;

/// A Schema is a collection of zero or more [Type]s.
///
/// Each type may refer to other types within the same schema,
/// or types imported into this schema from other schemas.
/// To instantiate a Schema, see [SchemaSystem].
#[derive(Debug, Clone)]
pub struct Schema {
    id: String,
    imports: Vec<Rc<Schema>>, //TODO: Use HashMap for imports
    types: Rc<TypeStore>,
}

impl Schema {
    pub(crate) fn new<A: AsRef<str>>(id: A, types: Rc<TypeStore>) -> Self {
        Self {
            id: id.as_ref().to_owned(),
            imports: vec![],
            types,
        }
    }

    /// Returns the id for this Schema
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Returns an [Import] representing all the types imported from
    /// the specified schema [id].
    fn import(&self, id: String) -> Option<Import> {
        todo!()
    }

    /// Returns an iterator over the imports of this [Schema].  Note that
    /// multiple ISL imports referencing the same schema id (to import
    /// individual types from the same schema id, for example) are
    /// represented by a single Import object.
    fn imports(&self) -> Box<dyn Iterator<Item = Import>> {
        todo!()
    }

    /// Returns the requested type, if present in this schema;
    /// otherwise returns None.
    fn get_type<A: AsRef<str>>(&self, name: A) -> Option<&TypeDefinition> {
        self.types.get_type_by_name(name.as_ref())
    }

    /// Returns an iterator over the types in this schema.
    pub(crate) fn get_types(&self) -> SchemaTypeIterator {
        SchemaTypeIterator::new(Rc::clone(&self.types))
    }

    /// Returns a new [Schema] instance containing all the types of this
    /// instance plus the provided type.  Note that the added type
    /// in the returned instance will hide a type of the same name
    /// from this instance.
    fn plus_type(&self, schema_type: TypeDefinition) -> Self {
        todo!()
    }
}

/// Provides an Iterator which returns [Type]s inside a [Schema]
pub struct SchemaTypeIterator {
    types: Rc<TypeStore>,
    index: usize,
}

impl SchemaTypeIterator {
    fn new(types: Rc<TypeStore>) -> Self {
        Self { types, index: 0 }
    }
}

impl Iterator for SchemaTypeIterator {
    type Item = TypeRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.types.get_types().len() {
            return None;
        }
        self.index = self.index + 1;
        Some(TypeRef::new(self.index - 1, Rc::clone(&self.types)))
    }
}

#[cfg(test)]
mod schema_tests {
    use super::*;
    use crate::system::Resolver;
    use ion_rs::value::owned::OwnedElement;
    use ion_rs::value::reader::element_reader;
    use ion_rs::value::reader::ElementReader;
    use rstest::*;
    use std::cell::RefCell;

    // helper function to be used by isl tests
    fn load(text: &str) -> Vec<OwnedElement> {
        element_reader()
            .read_all(text.as_bytes())
            .expect("parsing failed unexpectedly")
    }

    #[rstest(
    owned_elements, total_types,
    case::type_constraint_with_anonymous_type(
        /* For a schema with single anonymous type as below: 
            type:: { type: int }
         */
        load(r#" // For a schema with single anonymous type
            type:: {type: int}
        "#).into_iter(),
        2 // this includes the core type int and the anonymous type
    ),
    case::type_constraint_with_named_type(
        load(r#" For a schema with named type as below: 
            type:: { name: my_int, type: int }
        "#).into_iter(),
        2 // this includes the core type int and the anonymous type
    ),
    case::type_constraint_with_self_reference_type(
        load(r#" For a schema with self reference type as below: 
            type:: { name: my_int, type: my_int }
        "#).into_iter(),
        1 // this includes only my_int type
    ),
    case::type_constraint_with_nested_self_reference_type(
        load(r#" For a schema with nested self reference type as below:
            type:: { name: my_int, type: { type: my_int } }
        "#).into_iter(),
        2 // this includes my_int type and the anonymous type that uses my_int
    ),
    case::type_constraint_with_nested_type(
        load(r#" For a schema with nested types as below:
            type:: { name: my_int, type: { type: int } }
        "#).into_iter(),
        3 // this includes my_int type, the anonymous type that uses int and core type int
    ),
    case::type_constraint_with_nested_multiple_types(
        load(r#"  For a schema with nested multiple types as below: 
            type:: { name: my_int, type: { type: int }, type: { type: my_int } }
        "#).into_iter(),
        4 //  this includes my_int type, the anonymous type that uses int, core type int and the anonymous type that uses my_int type
    ),
    case::type_constraint_wiht_multiple_types(
        load(r#" For a schema with multiple type as below:
             type:: { name: my_int, type: int }
             type:: { name: my_bool, type: bool }
             type:: { type: string }
        "#).into_iter(),
        6
    ),
    case::all_of_constraint(
        load(r#" For a schema with all_of type as below: 
            type:: { all_of: [{ type: int }] }
        "#).into_iter(),
        3
    ),
    )]
    fn owned_elements_to_schema<'a, I: Iterator<Item = OwnedElement>>(
        owned_elements: I,
        total_types: usize,
    ) {
        // create a type_store and resolver instance to be used for loading OwnedElements as schema
        let type_store = &Rc::new(RefCell::new(TypeStore::new()));
        let mut resolver = Resolver::new(vec![]);

        // create a schema from owned_elements and verifies if the result is `ok`
        let schema = resolver.schema_from_elements(owned_elements, "my_schema.isl", type_store);
        assert_eq!(schema.is_ok(), true);

        // check if the types of the created schema matches with the actual types specified by test case
        let types: Vec<TypeRef> = schema.unwrap().get_types().collect();
        assert_eq!(types.len(), total_types);
    }
}
