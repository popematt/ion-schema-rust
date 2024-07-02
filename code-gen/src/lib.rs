extern crate core;

#[macro_use]
extern crate derive_builder;

use std::fmt::Debug;
use std::io;
use thiserror::Error;

mod generator;
mod model;
mod parse;

pub type Result<T> = std::result::Result<T, CodeGenError>;

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("{source:?}")]
    IoError {
        #[from]
        source: io::Error,
    },
    #[error("{0}")]
    Generator(String)
}
