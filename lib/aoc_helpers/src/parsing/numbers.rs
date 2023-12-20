use std::num::ParseIntError;

use super::*;

fn radix_number<Radix, T>(input: &str) -> IResult<&str, T>
where
    Radix: NumRadix,
    T: FromStrRadix,
{
    take_while1(Radix::is_digit_radix)
        .map(
            |number: &str| match FromStrRadix::from_str_radix(number, Radix::RADIX) {
                Ok(n) => n,
                Err(_) => panic!("FromStrRadix implementation failed to parse valid number"),
            },
        )
        .parse(input)
}

fn radix_number_m_n<'a, Radix, T>(m: usize, n: usize) -> impl FnMut(&'a str) -> IResult<&'a str, T>
where
    Radix: NumRadix,
    T: FromStrRadix,
{
    move |input: &str| {
        take_while_m_n(m, n, Radix::is_digit_radix)
            .map(
                |number: &str| match FromStrRadix::from_str_radix(number, Radix::RADIX) {
                    Ok(n) => n,
                    Err(_) => panic!("FromStrRadix implementation failed to parse valid number"),
                },
            )
            .parse(input)
    }
}

/// Parses a decimal number like "42"
pub fn decimal_number<T>(input: &str) -> IResult<&str, T>
where
    T: FromStrRadix,
{
    radix_number::<Dec, _>(input)
}

/// Parses a decimal number with at least `m` digits and at most `n` digits.
pub fn decimal_number_m_n<'a, T>(m: usize, n: usize) -> impl FnMut(&'a str) -> IResult<&'a str, T>
where
    T: FromStrRadix,
{
    radix_number_m_n::<Dec, _>(m, n)
}

pub fn hex_number<T>(input: &str) -> IResult<&str, T>
where
    T: FromStrRadix,
{
    radix_number::<Hex, _>(input)
}

pub fn hex_number_m_n<'a, T>(m: usize, n: usize) -> impl FnMut(&'a str) -> IResult<&'a str, T>
where
    T: FromStrRadix,
{
    radix_number_m_n::<Hex, _>(m, n)
}

pub fn single_digit_number<T>(input: &str) -> IResult<&str, T>
where
    T: FromStr,
    <T as FromStr>::Err: Debug,
{
    take_while_m_n(1, 1, |c: char| c.is_ascii_digit())
        .map(|number: &str| number.parse().expect("must parse"))
        .parse(input)
}

trait NumRadix {
    const RADIX: u32;

    fn is_digit_radix(c: impl AsChar) -> bool;
}

pub trait FromStrRadix: Sized {
    type Err;

    fn from_str_radix(src: &str, radix: u32) -> Result<Self, Self::Err>;
}

pub struct Dec;
impl NumRadix for Dec {
    const RADIX: u32 = 10;

    fn is_digit_radix(c: impl AsChar) -> bool {
        c.is_dec_digit()
    }
}

pub struct Hex;
impl NumRadix for Hex {
    const RADIX: u32 = 16;

    fn is_digit_radix(c: impl AsChar) -> bool {
        c.is_hex_digit()
    }
}

pub struct Oct;
impl NumRadix for Oct {
    const RADIX: u32 = 8;

    fn is_digit_radix(c: impl AsChar) -> bool {
        c.is_oct_digit()
    }
}

macro_rules! impl_from_str_radix_for_primitive {
    ($primitive:ty) => {
        impl FromStrRadix for $primitive {
            type Err = ParseIntError;

            fn from_str_radix(src: &str, radix: u32) -> Result<Self, Self::Err> {
                Self::from_str_radix(src, radix)
            }
        }
    };
}

impl_from_str_radix_for_primitive!(u8);
impl_from_str_radix_for_primitive!(u16);
impl_from_str_radix_for_primitive!(u32);
impl_from_str_radix_for_primitive!(u64);
impl_from_str_radix_for_primitive!(u128);

impl_from_str_radix_for_primitive!(i8);
impl_from_str_radix_for_primitive!(i16);
impl_from_str_radix_for_primitive!(i32);
impl_from_str_radix_for_primitive!(i64);
impl_from_str_radix_for_primitive!(i128);

impl_from_str_radix_for_primitive!(usize);
impl_from_str_radix_for_primitive!(isize);
