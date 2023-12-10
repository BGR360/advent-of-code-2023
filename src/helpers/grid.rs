use std::fmt;

pub use glam::{IVec2, UVec2};

use crate::helpers::math::CheckedSub;

/// A two-dimensional grid containing values of type `T` and indexed by values
/// of type `Pos`.
#[derive(Debug, Clone)]
pub struct Grid<T, Pos> {
    pub inner: grid::Grid<T>,
    /// The minimum position in the grid. Used to compute (row, col) offsets.
    pub min: Pos,
}

impl<T, Pos> Grid<T, Pos> {
    pub fn new(rows: usize, cols: usize) -> Self
    where
        T: Default,
        Pos: Default,
    {
        Self::from_inner(grid::Grid::new(rows, cols))
    }

    pub fn from_inner(inner: grid::Grid<T>) -> Self
    where
        Pos: Default,
    {
        Self {
            inner,
            min: Default::default(),
        }
    }

    pub fn from_vec(vec: Vec<T>, cols: usize) -> Self
    where
        Pos: Default,
    {
        Self::from_inner(grid::Grid::from_vec(vec, cols))
    }
}

pub trait GridIndex: Copy + Sized {
    fn into_row_col(self, min: Self) -> Option<(usize, usize)>;
    fn from_row_col(row: usize, col: usize, min: Self) -> Option<Self>;
}

impl<T, Pos> Grid<T, Pos>
where
    Pos: GridIndex,
{
    #[inline]
    pub fn get(&self, pos: Pos) -> Option<&T> {
        let (row, col) = self.make_row_col(pos)?;
        self.inner.get(row, col)
    }

    #[inline]
    pub fn get_mut(&mut self, pos: Pos) -> Option<&mut T> {
        let (row, col) = self.make_row_col(pos)?;
        self.inner.get_mut(row, col)
    }

    /// Returns an iterator over the whole grid, starting from the first row and column.
    ///
    /// The iteration order is dependent on the internal memory layout.
    /// If you need a specific order, see [`iter_rows`](Grid::iter_rows) or
    /// [`iter_cols`](Grid::iter_cols).
    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.inner.iter()
    }

    /// Returns an mutable iterator over the whole grid that allows modifying each value.
    ///
    /// The iteration order is dependent on the internal memory layout.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> + '_ {
        self.inner.iter_mut()
    }

    /// Traverse the grid with positions for each cell.
    ///
    /// The iteration order is dependent on the internal memory layout,
    /// but the positions will be acurate either way.
    pub fn indexed_iter(&self) -> impl Iterator<Item = (Pos, &T)> + '_ {
        self.inner
            .indexed_iter()
            .map(|((row, col), t)| (self.make_pos(row, col).unwrap(), t))
    }

    #[inline]
    fn make_pos(&self, row: usize, col: usize) -> Option<Pos> {
        Pos::from_row_col(row, col, self.min)
    }

    #[inline]
    fn make_row_col(&self, pos: Pos) -> Option<(usize, usize)> {
        Pos::into_row_col(pos, self.min)
    }
}

impl GridIndex for UVec2 {
    #[inline(always)]
    fn into_row_col(self, min: Self) -> Option<(usize, usize)> {
        let row = self.y.checked_sub(min.y)?;
        let col = self.x.checked_sub(min.x)?;
        Some((row.try_into().ok()?, col.try_into().ok()?))
    }
    #[inline(always)]
    fn from_row_col(row: usize, col: usize, min: Self) -> Option<Self> {
        let row = row.try_into().ok()?;
        let col = col.try_into().ok()?;
        let pos = Self { x: col, y: row };

        pos.checked_sub(min)
    }
}

impl GridIndex for IVec2 {
    #[inline(always)]
    fn into_row_col(self, min: Self) -> Option<(usize, usize)> {
        let row = self.y.checked_sub(min.y)?;
        let col = self.x.checked_sub(min.x)?;
        Some((row.try_into().ok()?, col.try_into().ok()?))
    }
    #[inline(always)]
    fn from_row_col(row: usize, col: usize, min: Self) -> Option<Self> {
        let pos = Self {
            x: col.try_into().ok()?,
            y: row.try_into().ok()?,
        };

        pos.checked_sub(min)
    }
}

impl<T, Pos> PartialEq for Grid<T, Pos>
where
    T: Eq,
    Pos: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner) && self.min == other.min
    }
}

impl<T, Pos> Eq for Grid<T, Pos>
where
    T: Eq,
    Pos: Eq,
{
}

impl<T, Pos> From<grid::Grid<T>> for Grid<T, Pos>
where
    Pos: Default,
{
    fn from(inner: grid::Grid<T>) -> Self {
        Self::from_inner(inner)
    }
}

impl<T, Pos> fmt::Display for Grid<T, Pos>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first_row = true;
        for row in 0..self.inner.rows() {
            if !first_row {
                writeln!(f)?;
            }

            for col in 0..self.inner.cols() {
                write!(f, "{}", self.inner.get(row, col).unwrap())?;
            }

            first_row = false;
        }
        Ok(())
    }
}
