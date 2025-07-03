use crate::result::{HasIslSourceLocation, IonSchemaError, IonSchemaResult, IslSourceLocation};
use std::collections::BTreeSet;
use std::fmt::{Display, Formatter, Write};
use std::mem::{replace, swap};
use thiserror::Error;

/// Collects multiple errors to report them together.
///
/// ```ignore
/// # use ion_schema::IonSchemaResult;
/// #
/// use ion_schema::result::ErrorCollector;
///
/// fn error_prone_function(i: i8) -> IonSchemaResult<()> {
///     todo!()
/// }
/// fn some_fun() -> IonSchemaResult<()> {
///     // Create the ErrorCollector
///     let mut errors = ErrorCollector::default();
///     for i in 1..10 {
///         let Some(ok_value) = errors.ok_or_push_err(error_prone_function(i)) else {
///             continue;
///         }
///         // Do something with `ok_value`
///     }
///     // Transform the error collector into a result.
///     errors.into_result_with(())
/// }
/// ```
#[derive(Debug, Default)]
pub(crate) struct InvalidSchemaErrorCollector {
    errors: BTreeSet<InvalidSchemaDetail>,
}
impl InvalidSchemaErrorCollector {
    /// Consumes `self` and `ok_value`. If this `ErrorCollector` contains no errors, returns `Ok(ok_value)`.
    /// Otherwise, returns `Err`.
    ///
    /// Use this method to turn the `ErrorCollector` into a usable `IonSchemaResult`.
    pub(crate) fn into_result_with<T>(self, ok_value: T) -> Result<T, InvalidSchemaError> {
        if self.errors.is_empty() {
            Ok(ok_value)
        } else {
            Err(InvalidSchemaError(InvalidSchemaDetails::Many(
                self.errors,
            )))
        }
    }

    pub(crate) fn into_partial_result_with<T>(
        self,
        ok_value: T,
    ) -> Result<T, (Option<T>, InvalidSchemaError)> {
        if self.errors.is_empty() {
            Ok(ok_value)
        } else {
            Err((
                Some(ok_value),
                InvalidSchemaError(InvalidSchemaDetails::Many(self.errors)),
            ))
        }
    }

    /// Push an `IonSchemaError` to this `ErrorCollector`.
    pub(crate) fn push_err(&mut self, e: impl Into<InvalidSchemaError>) {
        let e = e.into();
        match e.0 {
            InvalidSchemaDetails::Zero => {}
            InvalidSchemaDetails::Many(errors) => self.errors.extend(errors),
            InvalidSchemaDetails::One(error) => {
                self.errors.insert(error);
            }
        }
    }

    /// Converts from [`IonSchemaResult<T>`] to [`Option<T>`].
    ///
    /// Converts `result` into an [`Option<T>`], consuming `result`, and pushing the error, if any,
    /// to this `ErrorCollector`.
    /// This is the multi-error equivalent to [`Result::ok`].
    pub(crate) fn ok_or_push_err<T>(
        &mut self,
        result: impl Into<Result<T, InvalidSchemaError>>,
    ) -> Option<T> {
        match result.into() {
            Ok(value) => Some(value),
            Err(e) => {
                self.push_err(e);
                None
            }
        }
    }
}

pub(crate) trait IntoErrorCollector<T> {
    fn collect_errors<Dest: FromIterator<T>>(self) -> Result<Dest, InvalidSchemaError>;
}
impl<T, I> IntoErrorCollector<T> for I
where
    I: IntoIterator<Item = Result<T, InvalidSchemaError>>,
{
    fn collect_errors<Dest: FromIterator<T>>(self) -> Result<Dest, InvalidSchemaError> {
        let mut error_collector = InvalidSchemaErrorCollector::default();
        let ok = self
            .into_iter()
            .filter_map(|r| match r {
                Ok(t) => Some(t),
                Err(e) => {
                    error_collector.push_err(e);
                    None
                }
            })
            .collect();
        error_collector.into_result_with(ok)
    }
}

#[derive(Debug, Error, PartialEq)]
#[error(transparent)]
pub struct InvalidSchemaError(InvalidSchemaDetails);

impl InvalidSchemaError {
    pub(crate) fn builder(description: String) -> InvalidSchemaDetailBuilder {
        InvalidSchemaDetailBuilder::new(description)
    }

    pub(crate) fn with_schema_id(self, schema_id: impl Into<String> + Clone) -> Self {
        match self.0 {
            InvalidSchemaDetails::Zero => Self(InvalidSchemaDetails::Zero),
            InvalidSchemaDetails::One(detail) => Self(InvalidSchemaDetails::One(
                detail.with_schema_id(schema_id.into()),
            )),
            InvalidSchemaDetails::Many(details) => {
                let details = details
                    .into_iter()
                    .map(|detail| detail.with_schema_id(schema_id.clone().into()))
                    .collect();
                Self(InvalidSchemaDetails::Many(details))
            }
        }
    }
}

pub struct InvalidSchemaDetailBuilder {
    schema_id: String,
    row_column: IslSourceLocation,
    description: String,
}

impl InvalidSchemaDetailBuilder {
    fn new(description: String) -> Self {
        InvalidSchemaDetailBuilder {
            description,
            row_column: IslSourceLocation::default(),
            schema_id: "".to_string(),
        }
    }
    pub(crate) fn with_schema_id(mut self, id: impl Into<String>) -> Self {
        self.schema_id = id.into();
        self
    }
    pub(crate) fn with_isl_source_location<T: HasIslSourceLocation>(mut self, item: &T) -> Self {
        self.row_column = item.isl_source_location();
        self
    }
    pub(crate) fn build(self) -> InvalidSchemaError {
        InvalidSchemaError(InvalidSchemaDetails::One(InvalidSchemaDetail {
            schema_id: self.schema_id,
            row_column: self.row_column,
            description: self.description,
        }))
    }
}
impl From<InvalidSchemaDetailBuilder> for InvalidSchemaError {
    fn from(value: InvalidSchemaDetailBuilder) -> Self {
        value.build()
    }
}
impl<T> From<InvalidSchemaDetailBuilder> for Result<T, InvalidSchemaError> {
    fn from(value: InvalidSchemaDetailBuilder) -> Self {
        Err(value.build())
    }
}
impl From<InvalidSchemaDetailBuilder> for IonSchemaError {
    fn from(value: InvalidSchemaDetailBuilder) -> Self {
        IonSchemaError::InvalidSchemaError(value.build())
    }
}
impl<T> From<InvalidSchemaDetailBuilder> for IonSchemaResult<T> {
    fn from(value: InvalidSchemaDetailBuilder) -> Self {
        Err(IonSchemaError::InvalidSchemaError(value.build()))
    }
}

#[derive(Debug, Error, PartialEq)]
enum InvalidSchemaDetails {
    Zero,
    One(InvalidSchemaDetail),
    Many(BTreeSet<InvalidSchemaDetail>),
}
impl InvalidSchemaDetails {

    fn append(self, other: Self) -> Self {
        use InvalidSchemaDetails::*;
        match (self, other) {
            (this, Zero) => this,
            (Zero, other) => other,
            (One(detail), One(other_detail)) => {
                let mut details = BTreeSet::new();
                details.insert(detail);
                details.insert(other_detail);
                Many(details)
            }
            (Many(mut details), One(detail)) |
            (One(detail), Many(mut details)) => {
                details.insert(detail);
                Many(details)
            }
            (Many(mut details), Many(mut other_details)) => {
                details.append(&mut other_details);
                Many(details)
            }
        }
    }
}
impl Display for InvalidSchemaDetails {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidSchemaDetails::Zero => f.write_str("<no errors>"),
            InvalidSchemaDetails::One(one) => Display::fmt(one, f),
            InvalidSchemaDetails::Many(many) => {
                for detail in many {
                    Display::fmt(detail, f)?;
                    f.write_char('\n')?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Error, PartialEq, PartialOrd, Eq, Ord)]
#[error("{schema_id}:{row_column} > {description}")]
struct InvalidSchemaDetail {
    schema_id: String,
    row_column: IslSourceLocation,
    description: String,
}
impl InvalidSchemaDetail {
    fn with_schema_id(mut self, schema_id: String) -> Self {
        self.schema_id = schema_id;
        self
    }
}

pub(crate) trait ErrorAppender {
    fn append(&mut self, other: InvalidSchemaError);
}
impl ErrorAppender for InvalidSchemaError {
    fn append(&mut self, mut other: InvalidSchemaError) {
        use InvalidSchemaDetails::*;

        if let (One(a), Many(b)) = (&mut self.0, &mut other.0) {
            swap(&mut self.0, &mut other.0);
        }

        if let One(_) = &other.0 {

        }
    }
}
