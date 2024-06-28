//! # Parser Combinators
//!
//! This is not a very efficient implementation, but maybe enough for simple
//! tasks.
//!
//! ## Reference
//!
//! - [zhihu](https://zhuanlan.zhihu.com/p/76283535)
//! - [chumsky](https://github.com/zesterer/chumsky/tree/main)

use std::{rc::Rc, str::Chars};

use super::diagnostic::{Diagnostic, DiagnosticList};

#[derive(Clone)]
pub struct Input<'a> {
    src: Chars<'a>,
    offset: usize,
}

pub trait PositionalInput {
    fn offset(&self) -> usize;
}

pub struct Extra<'a, S> {
    diag: &'a mut DiagnosticList,
    pub state: &'a mut S,
}

impl<'a, S> Extra<'a, S> {
    pub fn new(diag: &'a mut DiagnosticList, state: &'a mut S) -> Self { Self { diag, state } }

    pub fn add_diagnostic(&mut self, snippet: Diagnostic) { self.diag.push(snippet); }
}

impl<'a> Input<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src: src.chars(),
            offset: 0,
        }
    }

    pub fn as_str(&self) -> &'a str { self.src.as_str() }

    pub fn advance(&mut self, n: usize) {
        for _ in 0..n {
            self.src.next();
        }
    }
}

impl<'a> PositionalInput for Input<'a> {
    fn offset(&self) -> usize { self.offset }
}

impl<'a> Iterator for Input<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.src.next()?;
        self.offset += ch.len_utf8();
        Some(ch)
    }
}

pub struct Checkpoint<I> {
    input: I,
    num_diagnostics: usize,
}

impl<I> Checkpoint<I>
where
    I: Clone,
{
    pub fn new<S>(input: &I, extra: &Extra<S>) -> Self {
        Self {
            input: input.clone(),
            num_diagnostics: extra.diag.len(),
        }
    }

    pub fn restore<S>(self, input: &mut I, extra: &mut Extra<S>) {
        *input = self.input;
        extra.diag.truncate(self.num_diagnostics);
    }
}

pub trait Parser<I, S> {
    type Output;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output>;
}

impl<I, S, P> Parser<I, S> for &P
where
    P: Parser<I, S>,
{
    type Output = P::Output;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        (**self).parse(input, extra)
    }
}

pub struct Just<S> {
    ch: char,
    _marker: std::marker::PhantomData<S>,
}

impl<'a, S> Parser<Input<'a>, S> for Just<S> {
    type Output = char;

    fn parse(&self, input: &mut Input<'a>, extra: &mut Extra<S>) -> Option<Self::Output> {
        let pos = input.offset();
        match input.next() {
            Some(ch) if ch == self.ch => Some(ch),
            Some(ch) => {
                let snippet = Diagnostic::error("invalid character")
                    .annotate(pos..pos, format!("expected {:?}, found {:?}", self.ch, ch));
                extra.diag.push(snippet);
                None
            }
            None => {
                let snippet = Diagnostic::error("unexpected end of input")
                    .annotate(pos..pos, "expected a character");
                extra.diag.push(snippet);
                None
            }
        }
    }
}

pub struct Str<S> {
    s: String,
    _marker: std::marker::PhantomData<S>,
}

impl<'a, S> Parser<Input<'a>, S> for Str<S> {
    type Output = &'a str;

    fn parse(&self, input: &mut Input<'a>, extra: &mut Extra<S>) -> Option<Self::Output> {
        let pos = input.offset();
        let src = input.as_str();
        if src.starts_with(&self.s) {
            input.take(self.s.len()).for_each(|_| ());
            Some(&src[..self.s.len()])
        } else {
            let snippet = Diagnostic::error("invalid string").annotate(
                pos..pos,
                format!(
                    "expected {:?}, found {:?}",
                    self.s,
                    &src[..self.s.len().min(src.len())]
                ),
            );
            extra.diag.push(snippet);
            None
        }
    }
}

pub struct UntilStr<S> {
    s: String,
    _marker: std::marker::PhantomData<S>,
}

impl<'a, S> Parser<Input<'a>, S> for UntilStr<S> {
    type Output = &'a str;

    fn parse(&self, input: &mut Input<'a>, extra: &mut Extra<S>) -> Option<Self::Output> {
        let pos = input.offset();
        let src = input.as_str();
        if let Some(end) = src.find(&self.s) {
            let result = &src[..end];
            input.advance(end);
            Some(result)
        } else {
            let snippet = Diagnostic::error("unexpected end of input").annotate(
                pos + src.len()..pos + src.len(),
                format!("missing {:?}", self.s),
            );
            extra.diag.push(snippet);
            None
        }
    }
}

pub struct Satisfy<F, S>
where
    F: Fn(char) -> bool,
{
    f: F,
    _marker: std::marker::PhantomData<S>,
}

impl<'a, F, S> Parser<Input<'a>, S> for Satisfy<F, S>
where
    F: Fn(char) -> bool,
{
    type Output = char;

    fn parse(&self, input: &mut Input<'a>, extra: &mut Extra<S>) -> Option<Self::Output> {
        let pos = input.offset();
        match input.next() {
            Some(ch) if (self.f)(ch) => Some(ch),
            Some(ch) => {
                let snippet = Diagnostic::error("invalid character")
                    .annotate(pos..pos, format!("unexpected character {:?}", ch));
                extra.diag.push(snippet);
                None
            }
            None => {
                let snippet = Diagnostic::error("unexpected end of input")
                    .annotate(pos..pos, "expected a character");
                extra.diag.push(snippet);
                None
            }
        }
    }
}

pub struct Or<P, Q>(P, Q);

impl<P, Q, I, S> Parser<I, S> for Or<P, Q>
where
    I: Clone,
    P: Parser<I, S>,
    Q: Parser<I, S, Output = P::Output>,
{
    type Output = P::Output;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        let ckpt = Checkpoint::new(input, extra);
        let o = self.0.parse(input, extra);
        if o.is_some() {
            return o;
        }
        ckpt.restore(input, extra);
        self.1.parse(input, extra)
    }
}

pub struct Map<P, F>(P, F);

impl<P, F, I, S, B> Parser<I, S> for Map<P, F>
where
    P: Parser<I, S>,
    F: Fn(P::Output) -> B,
{
    type Output = B;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        self.0.parse(input, extra).map(&self.1)
    }
}

/// Map a parser with the extra information.
pub struct MapWith<P, F>(P, F);

impl<P, F, I, S, B> Parser<I, S> for MapWith<P, F>
where
    P: Parser<I, S>,
    F: Fn(P::Output, &mut Extra<S>) -> B,
{
    type Output = B;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        self.0.parse(input, extra).map(|o| self.1(o, extra))
    }
}

pub struct AndL<P, Q>(P, Q);

impl<P, Q, I, S> Parser<I, S> for AndL<P, Q>
where
    P: Parser<I, S>,
    Q: Parser<I, S>,
{
    type Output = P::Output;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        let o = self.0.parse(input, extra)?;
        self.1.parse(input, extra)?;
        Some(o)
    }
}

pub struct AndR<P, Q>(P, Q);

impl<P, Q, I, S> Parser<I, S> for AndR<P, Q>
where
    P: Parser<I, S>,
    Q: Parser<I, S>,
{
    type Output = Q::Output;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        self.0.parse(input, extra)?;
        self.1.parse(input, extra)
    }
}

pub struct And<P, Q>(P, Q);

impl<P, Q, I, S> Parser<I, S> for And<P, Q>
where
    P: Parser<I, S>,
    Q: Parser<I, S>,
{
    type Output = (P::Output, Q::Output);

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        let o1 = self.0.parse(input, extra)?;
        let o2 = self.1.parse(input, extra)?;
        Some((o1, o2))
    }
}

pub struct Between<P, Q, R>(P, Q, R);

impl<P, Q, R, I, S> Parser<I, S> for Between<P, Q, R>
where
    P: Parser<I, S>,
    Q: Parser<I, S>,
    R: Parser<I, S>,
{
    type Output = Q::Output;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        self.0.parse(input, extra)?;
        let o = self.1.parse(input, extra)?;
        self.2.parse(input, extra)?;
        Some(o)
    }
}

pub struct Map2<P, Q, F>(P, Q, F);

impl<P, Q, F, I, S, B> Parser<I, S> for Map2<P, Q, F>
where
    P: Parser<I, S>,
    Q: Parser<I, S>,
    F: Fn(P::Output, Q::Output) -> B,
{
    type Output = B;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        let o1 = self.0.parse(input, extra)?;
        let o2 = self.1.parse(input, extra)?;
        Some((self.2)(o1, o2))
    }
}

pub struct Map2With<P, Q, F>(P, Q, F);

impl<P, Q, F, I, S, B> Parser<I, S> for Map2With<P, Q, F>
where
    P: Parser<I, S>,
    Q: Parser<I, S>,
    F: Fn(P::Output, Q::Output, &mut Extra<S>) -> B,
{
    type Output = B;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        let o1 = self.0.parse(input, extra)?;
        let o2 = self.1.parse(input, extra)?;
        Some(self.2(o1, o2, extra))
    }
}

pub struct Many<P>(P);

impl<P, I, S> Parser<I, S> for Many<P>
where
    I: Clone,
    P: Parser<I, S>,
{
    type Output = Vec<P::Output>;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        let mut v = Vec::new();
        let mut ckpt = Checkpoint::new(input, extra);
        while let Some(o) = self.0.parse(input, extra) {
            v.push(o);
            ckpt = Checkpoint::new(input, extra);
        }
        // the last parse failed, restore the previous checkpoint
        ckpt.restore(input, extra);
        Some(v)
    }
}

pub struct Many1<P>(P);

impl<P, I, S> Parser<I, S> for Many1<P>
where
    I: Clone,
    P: Parser<I, S>,
{
    type Output = Vec<P::Output>;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        let mut v = Vec::new();
        if let Some(o) = self.0.parse(input, extra) {
            v.push(o); // the first parse must succeed, so no ckpt needed

            // the rest can fail, so we need to keep track of the last successful parse
            let mut ckpt = Checkpoint::new(input, extra);
            while let Some(o) = self.0.parse(input, extra) {
                v.push(o);
                ckpt = Checkpoint::new(input, extra);
            }
            // the last parse failed, restore the previous checkpoint
            ckpt.restore(input, extra);
            Some(v)
        } else {
            // the first parse failed, the whole process fails
            None
        }
    }
}

pub struct Optional<P>(P);

impl<P, I, S> Parser<I, S> for Optional<P>
where
    I: Clone,
    P: Parser<I, S>,
{
    type Output = Option<P::Output>;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        let ckpt = Checkpoint::new(input, extra);
        match self.0.parse(input, extra) {
            Some(o) => Some(Some(o)),
            None => {
                ckpt.restore(input, extra);
                Some(None)
            }
        }
    }
}

pub struct Choice<T>(T);

// implement choice for given tuple
macro_rules! impl_choice {
    () => {};
    ($head:ident $($tail:ident)*) => {
        impl<$head, $($tail,)* I, S> Parser<I, S> for Choice<($head, $($tail),*)>
        where
            I: Clone,
            $head: Parser<I, S>,
            $($tail: Parser<I, S, Output = $head::Output>,)*
        {
            type Output = $head::Output;

            fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
                #[allow(non_snake_case)]
                let ($head, $($tail),*) = &self.0;
                #[allow(unused_variables)]
                let ckpt = Checkpoint::new(input, extra);
                if let Some(o) = $head.parse(input, extra) {
                    return Some(o);
                }
                $(
                    ckpt.restore(input, extra);
                    #[allow(unused_variables)]
                    let ckpt = Checkpoint::new(input, extra);
                    if let Some(o) = $tail.parse(input, extra) {
                        return Some(o);
                    }
                )*
                // the last one should not restore the checkpoint
                None
            }
        }

        impl_choice!($($tail)*);
    };
}

// reference: [chumsky](https://github.com/zesterer/chumsky/blob/main/src/primitive.rs)
// the maximum number of parsers in a choice is 26, maybe extend later if needed
impl_choice!(_A _B _C _D _E _F _G _H _I _J _K _L _M _N _O _P _Q _R _S _T _U _V _W _X _Y _Z);

type RecFn<'a, I, S, P, O> = dyn for<'f> Fn(&'f P) -> Box<dyn Parser<I, S, Output = O> + 'f> + 'a;

pub struct Rec<'a, I, S, O>(Rc<RecFn<'a, I, S, Self, O>>);

impl<'a, I, S, O> Rec<'a, I, S, O> {
    pub fn new<F>(f: F) -> Self
    where
        F: for<'f> Fn(&'f Self) -> Box<dyn Parser<I, S, Output = O> + 'f> + 'a,
    {
        Self(Rc::new(f))
    }
}

impl<'a, I, S, O> Parser<I, S> for Rec<'a, I, S, O> {
    type Output = O;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        (self.0)(self).parse(input, extra)
    }
}

pub struct SepBy<P, Q>(P, Q);

impl<P, Q, I, S> Parser<I, S> for SepBy<P, Q>
where
    I: Clone,
    P: Parser<I, S>,
    Q: Parser<I, S>,
{
    type Output = Vec<P::Output>;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        let mut v = Vec::new();
        let ckpt = Checkpoint::new(input, extra);
        if let Some(o) = self.0.parse(input, extra) {
            v.push(o);
            loop {
                let ckpt = Checkpoint::new(input, extra);
                if self.1.parse(input, extra).is_none() {
                    ckpt.restore(input, extra);
                    break;
                }
                if let Some(o) = self.0.parse(input, extra) {
                    v.push(o);
                } else {
                    // allow the last redundant separator, so restore if the elem parser fails
                    // this will restore to the position before the separator
                    ckpt.restore(input, extra);
                    break;
                }
            }
        } else {
            // allow empty list
            ckpt.restore(input, extra);
        }
        Some(v)
    }
}

pub struct Spanned<P>(P);

impl<P, I, S> Parser<I, S> for Spanned<P>
where
    I: Clone + PositionalInput,
    P: Parser<I, S>,
{
    type Output = (P::Output, usize, usize);

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        let before = input.offset();
        let o = self.0.parse(input, extra)?;
        let after = input.offset();
        Some((o, before, after))
    }
}

pub struct Then<P, F>(P, F);

impl<P, F, I, S, O> Parser<I, S> for Then<P, F>
where
    I: Clone,
    P: Parser<I, S>,
    F: Fn(P::Output) -> Option<O>,
{
    type Output = O;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        self.0.parse(input, extra).and_then(&self.1)
    }
}

pub struct ThenWith<P, F>(P, F);

impl<P, F, I, S, O> Parser<I, S> for ThenWith<P, F>
where
    I: Clone,
    P: Parser<I, S>,
    F: Fn(P::Output, &mut Extra<S>) -> Option<O>,
{
    type Output = O;

    fn parse(&self, input: &mut I, extra: &mut Extra<S>) -> Option<Self::Output> {
        self.0.parse(input, extra).and_then(|o| (self.1)(o, extra))
    }
}

pub trait ParserCombine<I, S>: Parser<I, S> {
    fn or<Q>(self, other: Q) -> Or<Self, Q>
    where
        Self: Sized,
        Q: Parser<I, S, Output = Self::Output>,
    {
        Or(self, other)
    }

    fn and<Q>(self, other: Q) -> And<Self, Q>
    where
        Self: Sized,
        Q: Parser<I, S>,
    {
        And(self, other)
    }

    fn and_l<Q>(self, other: Q) -> AndL<Self, Q>
    where
        Self: Sized,
        Q: Parser<I, S>,
    {
        AndL(self, other)
    }

    fn and_r<Q>(self, other: Q) -> AndR<Self, Q>
    where
        Self: Sized,
        Q: Parser<I, S>,
    {
        AndR(self, other)
    }

    fn many(self) -> Many<Self>
    where
        Self: Sized,
    {
        Many(self)
    }

    fn many1(self) -> Many1<Self>
    where
        Self: Sized,
    {
        Many1(self)
    }

    fn optional(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional(self)
    }

    fn map<F, O>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> O,
    {
        Map(self, f)
    }

    fn map_with<F, O>(self, f: F) -> MapWith<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output, &mut Extra<S>) -> O,
    {
        MapWith(self, f)
    }

    fn to<O>(self, o: O) -> Map<Self, impl Fn(Self::Output) -> O>
    where
        Self: Sized,
        O: Copy,
    {
        Map(self, move |_| o)
    }

    fn between<L, R>(self, left: L, right: R) -> Between<L, Self, R>
    where
        Self: Sized,
        L: Parser<I, S>,
        R: Parser<I, S>,
    {
        Between(left, self, right)
    }

    fn sep_by<Q>(self, sep: Q) -> SepBy<Self, Q>
    where
        Self: Sized,
        Q: Parser<I, S>,
    {
        SepBy(self, sep)
    }

    fn spanned(self) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned(self)
    }

    fn then<F, O>(self, f: F) -> Then<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> Option<O>,
    {
        Then(self, f)
    }

    fn then_with<F, O>(self, f: F) -> ThenWith<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output, &mut Extra<S>) -> Option<O>,
    {
        ThenWith(self, f)
    }

    fn discard(self) -> Map<Self, impl Fn(Self::Output)>
    where
        Self: Sized,
    {
        Map(self, |_| ())
    }
}

impl<I, S, P> ParserCombine<I, S> for P where P: Parser<I, S> {}

pub fn just<S>(ch: char) -> Just<S> {
    Just {
        ch,
        _marker: std::marker::PhantomData,
    }
}

pub fn string<S>(s: impl Into<String>) -> Str<S> {
    Str {
        s: s.into(),
        _marker: std::marker::PhantomData,
    }
}

pub fn choice<T>(t: T) -> Choice<T> { Choice(t) }

pub fn satisfy<F, S>(f: F) -> Satisfy<F, S>
where
    F: Fn(char) -> bool,
{
    Satisfy {
        f,
        _marker: std::marker::PhantomData,
    }
}

pub fn rec<'a, I, S, O>(
    f: impl for<'f> Fn(&'f Rec<'a, I, S, O>) -> Box<dyn Parser<I, S, Output = O> + 'f> + 'a,
) -> Rec<'a, I, S, O> {
    Rec::new(f)
}

pub fn take_while<'a, F, S>(f: F) -> impl Parser<Input<'a>, S, Output = String>
where
    F: Fn(char) -> bool,
{
    satisfy(f).many().map(|v| v.into_iter().collect())
}

pub fn take_while1<'a, F, S>(f: F) -> impl Parser<Input<'a>, S, Output = String>
where
    F: Fn(char) -> bool,
{
    satisfy(f).many1().map(|v| v.into_iter().collect())
}

pub fn until_str<S>(s: impl Into<String>) -> UntilStr<S> {
    UntilStr {
        s: s.into(),
        _marker: std::marker::PhantomData,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::collections::diagnostic::RenderOptions;

    #[test]
    fn test_just() {
        let mut input = Input::new("a");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = just('a');
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some('a'));
        assert_eq!(input.offset(), 1);
        assert!(extra.diag.is_empty());

        let mut input = Input::new("b");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = just('a');
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, None);
        assert_eq!(extra.diag.len(), 1);
    }

    #[test]
    fn test_strng() {
        let mut input = Input::new("abc");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = string("abc");
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some("abc"));
        assert_eq!(input.offset(), 3);
        assert!(extra.diag.is_empty());

        let mut input = Input::new("def");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = string("abc");
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, None);
        assert_eq!(input.offset(), 0);
        assert_eq!(extra.diag.len(), 1);
    }

    #[test]
    fn test_satisfy() {
        let mut input = Input::new("a");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = satisfy(|ch| ch == 'a');
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some('a'));
        assert_eq!(input.offset(), 1);
        assert!(extra.diag.is_empty());

        let mut input = Input::new("b");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = satisfy(|ch| ch == 'a');
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, None);
        assert_eq!(input.offset(), 1); // because we consumed the character
        assert_eq!(extra.diag.len(), 1);
    }

    #[test]
    fn test_or() {
        let mut input = Input::new("a");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = just('a').or(just('b'));
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some('a'));
        assert_eq!(input.offset(), 1);
        assert!(extra.diag.is_empty());

        let mut input = Input::new("b");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = just('a').or(just('b'));
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some('b'));
        assert_eq!(input.offset(), 1);
        assert!(extra.diag.is_empty());

        let mut input = Input::new("c");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = just('a').or(just('b'));
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, None);
        assert_eq!(input.offset(), 1); // because we consumed the character
        assert_eq!(extra.diag.len(), 1); // TODO: now only the second parser is
                                         // reported, but we should report both
                                         // parsers
    }

    #[test]
    fn test_map_with() {
        // we can use a counter as state, to count the number of alphabetic characters
        let mut input = Input::new("abc");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut 0,
        };
        let parser = satisfy(|ch| ch.is_alphabetic())
            .map_with(|ch, counter| {
                *counter.state += 1;
                ch
            })
            .many();

        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some(vec!['a', 'b', 'c']));
        assert_eq!(input.offset(), 3);
        assert_eq!(*extra.state, 3);
    }

    #[test]
    fn test_and() {
        // test andl, andr, and
        let mut input = Input::new("abc");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = And(just('a'), just('b'));
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some(('a', 'b')));

        let mut input = Input::new("abc");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = AndL(just('a'), just('b'));
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some('a'));

        let mut input = Input::new("abc");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = AndR(just('a'), just('b'));
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some('b'));
    }

    #[test]
    fn test_choice() {
        let mut input = Input::new("abc");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = choice((just('a'), just('b'), just('c')));
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some('a'));
        assert_eq!(input.offset(), 1);
        assert!(extra.diag.is_empty());

        let mut input = Input::new("bbc");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = choice((just('a'), just('b'), just('c')));
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some('b'));
        assert_eq!(input.offset(), 1);
        assert!(extra.diag.is_empty());

        let mut input = Input::new("cbc");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = choice((just('a'), just('b'), just('c')));
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, Some('c'));
        assert_eq!(input.offset(), 1);
        assert!(extra.diag.is_empty());

        let mut input = Input::new("dbc");
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = choice((just('a'), just('b'), just('c')));
        let o = parser.parse(&mut input, &mut extra);
        assert_eq!(o, None);
        assert_eq!(input.offset(), 1); // last one should not recover
        assert_eq!(extra.diag.len(), 1);
    }

    #[test]
    fn test_recursive() {
        let input = "[[a,1],1]";
        let mut input = Input::new(input);
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = rec(|p: &Rec<_, (), _>| {
            Box::new(
                just('[')
                    .and_r(p)
                    .and_l(just(','))
                    .and_l(just('1'))
                    .and_l(just(']'))
                    .or(just('a')),
            )
        });

        let o = parser.parse(&mut input, &mut extra);

        assert_eq!(o, Some('a'));
        assert_eq!(input.as_str(), "");
        assert_eq!(extra.diag.len(), 0);
    }

    #[test]
    fn test_sep_by() {
        let input = "a,b,c";
        let mut input = Input::new(input);
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = just('a').or(just('b')).sep_by(just(','));

        let o = parser.parse(&mut input, &mut extra);

        assert_eq!(o, Some(vec!['a', 'b']));
        assert_eq!(input.as_str(), ",c");
        assert_eq!(extra.diag.len(), 0);
    }

    #[test]
    fn test_take_while() {
        let input = "abc123";
        let mut input = Input::new(input);
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = take_while1(|ch| ch.is_alphabetic());

        let o = parser.parse(&mut input, &mut extra);

        assert_eq!(o, Some("abc".into()));
        assert_eq!(input.as_str(), "123");
        assert_eq!(extra.diag.len(), 0);
    }

    #[test]
    fn test_until_str() {
        let input = "abc123";
        let mut input = Input::new(input);
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = until_str("123");

        let o = parser.parse(&mut input, &mut extra);

        assert_eq!(o, Some("abc"));
        assert_eq!(input.as_str(), "123");
        assert_eq!(extra.diag.len(), 0);

        // failed case
        let input = "abc123";
        let mut input = Input::new(input);
        let mut extra = Extra {
            diag: &mut DiagnosticList::new(),
            state: &mut (),
        };
        let parser = until_str("456");

        let o = parser.parse(&mut input, &mut extra);

        assert_eq!(o, None);
        assert_eq!(input.as_str(), "abc123");
        assert_eq!(extra.diag.len(), 1);

        println!(
            "{}",
            extra.diag.render("abc123", &RenderOptions::unicode_round())
        );
    }
}
