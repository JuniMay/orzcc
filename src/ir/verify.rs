//! TODO: Verification on instructions, and blocks
//! - number of operands in instructions
//! - type of operands and results in instructions
//! - type of block parameters and arguments
//! - referred symbols' existence
//! - bit width/size of constants and types

use super::Context;

pub trait Verify {
    type Error;

    fn verify(&self, ctx: &Context) -> Result<(), Self::Error>;
}
