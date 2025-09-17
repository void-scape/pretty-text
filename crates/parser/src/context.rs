use winnow::Parser;
use winnow::combinator::*;
use winnow::error::{ContextError, ErrMode, StrContext, StrContextValue};
use winnow::stream::Stream;

pub type Error = ErrMode<ContextError>;

/// Wraps [`winnow`]'s context error reporting with short names.
pub trait ParserContext<'a, Input: Stream, Out> {
    /// Description of what is currently being parsed.
    fn label(self, label: &'static str) -> impls::Context<Self, Input, Out, Error, StrContext>
    where
        Self: Sized,
        Self: Parser<Input, Out, Error>;

    /// Description of what was expected.
    fn expected(
        self,
        description: &'static str,
    ) -> impls::Context<Self, Input, Out, Error, StrContext>
    where
        Self: Sized,
        Self: Parser<Input, Out, Error>;

    /// A [`&str`] token.
    fn expected_str(
        self,
        literal: &'static str,
    ) -> impls::Context<Self, Input, Out, Error, StrContext>
    where
        Self: Sized,
        Self: Parser<Input, Out, Error>;

    /// A [`char`] token.
    fn expected_char(self, literal: char) -> impls::Context<Self, Input, Out, Error, StrContext>
    where
        Self: Sized,
        Self: Parser<Input, Out, Error>;
}

impl<'a, P, Input, Out> ParserContext<'a, Input, Out> for P
where
    P: Parser<Input, Out, Error>,
    Input: Stream,
{
    fn label(self, label: &'static str) -> impls::Context<Self, Input, Out, Error, StrContext> {
        self.context(StrContext::Label(label))
    }

    fn expected(
        self,
        description: &'static str,
    ) -> impls::Context<Self, Input, Out, Error, StrContext> {
        self.context(StrContext::Expected(StrContextValue::Description(
            description,
        )))
    }

    fn expected_str(
        self,
        literal: &'static str,
    ) -> impls::Context<Self, Input, Out, Error, StrContext> {
        self.context(StrContext::Expected(StrContextValue::StringLiteral(
            literal,
        )))
    }

    fn expected_char(self, literal: char) -> impls::Context<Self, Input, Out, Error, StrContext> {
        self.context(StrContext::Expected(StrContextValue::CharLiteral(literal)))
    }
}
