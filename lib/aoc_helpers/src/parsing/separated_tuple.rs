use super::*;

pub fn separated_tuple<I, O, O2, E, List, G>(
    mut sep: G,
    mut t: List,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: Clone + InputLength,
    List: SeparatedTuple<I, O, E>,
    G: Parser<I, O2, E>,
    E: ParseError<I>,
{
    move |input: I| t.parse(input, |i| sep.parse(i))
}

/// Like [`tuple`] but ignores whitespace between elements.
pub fn ws_tuple<I, O, E: ParseError<I>, List: SeparatedTuple<I, O, E>>(
    l: List,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: Clone + InputLength + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    separated_tuple(space0, l)
}

/// Helper trait for the [`separated_tuple`] combinator.
pub trait SeparatedTuple<I, O, E> {
    /// Parses the input and returns a tuple of results of each parser.
    fn parse<O2, Sep: Parser<I, O2, E>>(&mut self, input: I, sep: Sep) -> IResult<I, O, E>;
}

impl<A, I, OA, E> SeparatedTuple<I, (OA,), E> for (A,)
where
    A: Parser<I, OA, E>,
    E: ParseError<I>,
{
    fn parse<O2, Sep: Parser<I, O2, E>>(&mut self, input: I, _sep: Sep) -> IResult<I, (OA,), E> {
        let (a,) = self;
        let (input, a) = a.parse(input)?;

        Ok((input, (a,)))
    }
}

macro_rules! impl_separated_tuple_trait {
    (
        $($field:ident $parser_ty:ident $output_ty:ident),*
        | $last_field:ident $last_parser_ty:ident $last_output_ty:ident
    ) => {
        impl<
            I,
            $($parser_ty, $output_ty),*,
            $last_parser_ty, $last_output_ty,
            E,
        > SeparatedTuple<
            I,
            ($($output_ty),*, $last_output_ty,),
            E,
        > for ($($parser_ty),*, $last_parser_ty)
        where
            I: Clone,
            $($parser_ty: Parser<I, $output_ty, E>),*,
            $last_parser_ty: Parser<I, $last_output_ty, E>,
            E: ParseError<I>,
        {
            fn parse<O2, Sep: Parser<I, O2, E>>(
                &mut self,
                input: I,
                mut sep: Sep,
            ) -> IResult<I, ($($output_ty),*, $last_output_ty), E> {
                let ($($field),*, $last_field) = self;
                $(
                    let (input, $field) = terminated(|i| $field.parse(i), |i| sep.parse(i))(input)?;
                )*
                let (input, $last_field) = $last_field.parse(input)?;

                Ok((input, ($($field),*, $last_field)))
            }
        }
    };
}

impl_separated_tuple_trait!(a A AO | b B BO);
impl_separated_tuple_trait!(
    a A AO, b B BO | c C CO
);
impl_separated_tuple_trait!(
    a A AO, b B BO, c C CO | d D DO
);
impl_separated_tuple_trait!(
    a A AO, b B BO, c C CO, d D DO | f F FO
);
impl_separated_tuple_trait!(
    a A AO, b B BO, c C CO, d D DO, f F FO | g G GO
);
impl_separated_tuple_trait!(
    a A AO, b B BO, c C CO, d D DO, f F FO, g G GO | h H HO
);
impl_separated_tuple_trait!(
    a A AO, b B BO, c C CO, d D DO, f F FO, g G GO, h H HO | j J JO
);
impl_separated_tuple_trait!(
    a A AO, b B BO, c C CO, d D DO, f F FO, g G GO, h H HO, j J JO | k K KO
);
impl_separated_tuple_trait!(
    a A AO, b B BO, c C CO, d D DO, f F FO, g G GO, h H HO, j J JO, k K KO | l L LO
);
impl_separated_tuple_trait!(
    a A AO, b B BO, c C CO, d D DO, f F FO, g G GO, h H HO, j J JO, k K KO, l L LO | m M MO
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_separated_tuple() {
        let mut parser = final_parser(ws_tuple((tag("a"), tag("b"), tag("c"))));

        parser("abc").unwrap();
        parser("a bc").unwrap();
        parser("ab c").unwrap();
        parser("a b c").unwrap();
        parser("a  b   c").unwrap();
        parser("a \tb\t c").unwrap();

        parser("abc ").unwrap_err();
        parser(" abc").unwrap_err();
    }
}
