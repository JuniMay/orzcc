use std::io;

use super::lexer::Lexer;

pub struct Parser<'a, T>
where
    T: io::Read,
{
    lexer: Lexer<'a, T>,
}

pub enum ParseError {}

impl<'a, T> Parser<'a, T>
where
    T: io::Read,
{
    pub fn new(input: &'a mut T) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(&mut self) -> Result<(), ParseError> {
        Ok(())
    }
}
