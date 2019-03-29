// TODO: WIP

use std::io::prelude::*;
use std::{error, io};

#[derive(Clone, Debug)]
enum Token<'a> {
    Id(&'a str),
}
type Input<'a> = &'a [Token<'a>];
type Error = ();
type ParseResult<'a, T> = Result<(T, Input<'a>), Error>;

trait Parse {
    type Output;

    fn parse<'a>(&self, input: Input<'a>) -> ParseResult<'a, Self::Output>;

    fn map<T, F>(&self, map: F) -> Map<&Self, F>
    where
        F: Fn(Self::Output) -> T,
    {
        Map { parser: self, map }
    }
}

struct Parser<F> {
    parse: F,
}

impl<F> Parser<F> {
    fn new<T>(parse: F) -> Self
    where
        F: Fn(Input) -> ParseResult<T>,
    {
        Parser { parse }
    }
}

impl<F, T> Parse for Parser<F>
where
    F: Fn(Input) -> ParseResult<T>,
{
    type Output = T;

    fn parse<'a>(&self, input: Input<'a>) -> ParseResult<'a, Self::Output> {
        (self.parse)(input)
    }
}

struct Map<P, F> {
    parser: P,
    map: F,
}

impl<P, F, T> Parse for Map<P, F>
where
    P: Parse,
    F: Fn(P::Output) -> T,
{
    type Output = T;

    fn parse<'a>(&self, input: Input<'a>) -> ParseResult<'a, Self::Output> {
        self.parser
            .parse(input)
            .map(|(res, inp)| ((self.map)(res), inp))
    }
}

fn tokenize(line: &str) -> Vec<Token> {
    line.split_whitespace().map(|s| Token::Id(s)).collect()
}

fn parse_any<'a>(inp: Input<'a>) -> ParseResult<'a, &'a Token<'a>> {
    inp.split_first().ok_or(())
}

fn main() -> Result<(), Box<error::Error>> {
    let any = Parser::new(parse_any);

    let parser = any;

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line?;
        let tokens = tokenize(line.as_ref());
        match parser.parse(&tokens) {
            Ok((res, inp)) => println!("{:?}", res),
            Err(err) => println!("{:?}", err),
        }
    }

    Ok(())
}
