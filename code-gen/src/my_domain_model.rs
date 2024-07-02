/// This enum is meant to flange the widgets in the endoplasmic reticulum.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum MyCoolEnum {
    Cold,
    Freezing,
    Chilly,
}
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AnyFloat {
    IonFloat(ion_rs::value::owned::Element),
    Float(f32),
    Double(f64),
}
#[derive(Debug, Clone, PartialEq, Builder)]
#[builder(setter(into))]
pub(crate) struct Person {
    age: ion_rs::types::integer::Integer,
    first_name: String,
    last_name: String,
}
