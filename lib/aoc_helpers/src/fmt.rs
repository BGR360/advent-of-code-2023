use std::{
    cell::Cell,
    fmt::{Display, Formatter, Result},
};

pub trait FmtExt {
    fn separated_by<T>(self, sep: T) -> SeparatedBy<Self::IntoIter, T>
    where
        Self: IntoIterator + Sized,
        Self::Item: Display,
        Self::IntoIter: Clone,
        T: Display,
    {
        SeparatedBy {
            iter: self.into_iter(),
            sep,
        }
    }

    fn terminated_by<T>(&self, term: T) -> TerminatedBy<'_, Self, T>
    where
        Self: Display + Sized,
        T: Display + Sized,
    {
        TerminatedBy { item: self, term }
    }

    fn repeated(&self, n: usize) -> Repeated<'_, Self>
    where
        Self: Display + Sized,
    {
        Repeated { item: self, n }
    }

    fn with_fmt<'a, F>(&'a self, fmt: F) -> WithFmt<'a, Self, F>
    where
        Self: Sized,
        F: FnMut(&mut Formatter<'_>, &'a Self) -> Result + 'a,
    {
        WithFmt::new(self, fmt)
    }
}

impl<T> FmtExt for T {}

pub struct SeparatedBy<Iter, Sep> {
    iter: Iter,
    sep: Sep,
}

impl<Iter, Sep> Display for SeparatedBy<Iter, Sep>
where
    Iter: Iterator + Clone,
    Iter::Item: Display,
    Sep: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut first = true;
        for item in self.iter.clone() {
            if !first {
                write!(f, "{}", self.sep)?;
            }
            first = false;

            write!(f, "{item}")?;
        }

        Ok(())
    }
}

pub struct TerminatedBy<'a, I, T> {
    item: &'a I,
    term: T,
}

impl<I, T> Display for TerminatedBy<'_, I, T>
where
    I: Display,
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}{}", self.item, self.term)
    }
}

pub struct Repeated<'a, T> {
    item: &'a T,
    n: usize,
}

impl<T> Display for Repeated<'_, T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for _ in 0..self.n {
            write!(f, "{}", self.item)?;
        }
        Ok(())
    }
}

pub struct WithFmt<'a, T, F> {
    item: &'a T,
    fmt: Cell<Option<F>>,
}

impl<'a, T, F> WithFmt<'a, T, F> {
    pub fn new(item: &'a T, fmt: F) -> Self {
        Self {
            item,
            fmt: Cell::new(Some(fmt)),
        }
    }
}

impl<'a, T, F> Display for WithFmt<'a, T, F>
where
    F: FnMut(&mut Formatter<'_>, &'a T) -> Result + 'a,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        let mut fmt = self.fmt.take().unwrap();
        (fmt)(f, self.item)?;
        self.fmt.set(Some(fmt));

        Ok(())
    }
}
