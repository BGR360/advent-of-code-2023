use std::{
    fmt,
    ops::{Index, IndexMut},
};

pub use glam::{IVec2, UVec2};

pub use aoc_helpers_derive::Tile;

use crate::math::{CheckedAdd, CheckedSub};

mod dir;
mod entry;
mod parse;

pub use dir::{Dir, Pos, Side};
pub use parse::parse_grid;

/// A two-dimensional grid containing values of type `T` and indexed by values
/// of type `Pos`.
#[derive(Debug, Clone)]
pub struct Grid<T, Pos = IVec2> {
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

    pub fn init(rows: usize, cols: usize, value: T) -> Self
    where
        T: Clone,
        Pos: Default,
    {
        Self::from_inner(grid::Grid::init(rows, cols, value))
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

    pub fn clear(&mut self)
    where
        T: Default,
    {
        self.inner.clear();
    }

    pub fn row_count(&self) -> usize {
        self.inner.rows()
    }

    pub fn col_count(&self) -> usize {
        self.inner.cols()
    }

    pub fn size(&self) -> (usize, usize) {
        (self.row_count(), self.col_count())
    }

    /// Returns a value implementing [`fmt::Display`] that will print the grid
    /// using the function `f` to convert each cell to a value that is
    /// printable.
    pub fn display_with<F, O>(&self, f: F) -> GridPrinter<'_, T, F>
    where
        F: Fn(&T) -> O,
        O: fmt::Display,
    {
        GridPrinter::new(self, f)
    }

    // pub fn resize(&mut self, rows: usize, cols: usize)
    // where
    //     T: Default,
    // {

    // }
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
    pub fn contains(&self, pos: Pos) -> bool {
        self.get(pos).is_some()
    }

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

    #[inline]
    pub fn pos_to_row_col(&self, pos: Pos) -> Option<(usize, usize)> {
        self.make_row_col(pos)
    }

    #[inline]
    pub fn row_col_to_pos(&self, row: usize, col: usize) -> Option<Pos> {
        self.make_pos(row, col)
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

    // pub fn entry(&mut self, pos: Pos) -> Entry<'_> {
    //     Entry::new(self)
    // }

    #[inline]
    fn make_pos(&self, row: usize, col: usize) -> Option<Pos> {
        Pos::from_row_col(row, col, self.min)
    }

    #[inline]
    fn make_row_col(&self, pos: Pos) -> Option<(usize, usize)> {
        Pos::into_row_col(pos, self.min)
    }
}

impl<T> Grid<T, Pos> {
    pub fn grow_to_fit(&mut self, pos: Pos)
    where
        T: Default + Clone,
    {
        if self.contains(pos) {
            return;
        }

        if self.size() == (0, 0) {
            let n_new_cols = pos.x.abs() + 1;
            let n_new_rows = pos.y.abs() + 1;

            let new_row = vec![T::default(); n_new_cols.try_into().unwrap()];

            for _ in 0..n_new_rows {
                self.inner.insert_row(0, new_row.clone());
            }

            self.min.x = self.min.x.min(pos.x);
            self.min.y = self.min.x.min(pos.y);
        } else {
            let min = self.min;
            let max = self
                .make_pos(self.row_count() - 1, self.col_count() - 1)
                .unwrap();

            if pos.x < min.x {
                let new_col = vec![T::default(); self.row_count()];
                let n_new_cols = min.x - pos.x;

                for _ in 0..n_new_cols {
                    self.inner.insert_col(0, new_col.clone());
                }

                self.min.x -= n_new_cols;
            } else if pos.x > max.x {
                let new_col = vec![T::default(); self.row_count()];
                let n_new_cols = pos.x - max.x;

                for _ in 0..n_new_cols {
                    self.inner.insert_col(self.col_count(), new_col.clone());
                }
            }

            let min = self.min;
            let max = self
                .make_pos(self.row_count() - 1, self.col_count() - 1)
                .unwrap();

            if pos.y < min.y {
                let new_row = vec![T::default(); self.col_count()];
                let n_new_rows = min.y - pos.y;

                for _ in 0..n_new_rows {
                    self.inner.insert_row(0, new_row.clone());
                }

                self.min.y -= n_new_rows;
            } else if pos.y > max.y {
                let new_row = vec![T::default(); self.col_count()];
                let n_new_rows = pos.y - max.y;

                for _ in 0..n_new_rows {
                    self.inner.insert_row(self.row_count(), new_row.clone());
                }
            }
        }
    }

    pub fn get_or_grow(&mut self, pos: Pos) -> &mut T
    where
        T: Default + Clone,
    {
        self.grow_to_fit(pos);
        self.get_mut(pos).unwrap()
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

        min.checked_add(pos)
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

impl<T, Pos> Index<Pos> for Grid<T, Pos>
where
    Pos: GridIndex,
{
    type Output = T;

    #[track_caller]
    fn index(&self, pos: Pos) -> &Self::Output {
        let (row, col) = self.make_row_col(pos).expect("index out of bounds");
        &self.inner[(row, col)]
    }
}

impl<T, Pos> IndexMut<Pos> for Grid<T, Pos>
where
    Pos: GridIndex,
{
    #[track_caller]
    fn index_mut(&mut self, pos: Pos) -> &mut Self::Output {
        let (row, col) = self.make_row_col(pos).expect("index out of bounds");
        &mut self.inner[(row, col)]
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

pub struct GridPrinter<'a, T, F> {
    grid: &'a grid::Grid<T>,
    mapper: F,
}

impl<'a, T, F> GridPrinter<'a, T, F> {
    pub fn new<Pos>(grid: &'a Grid<T, Pos>, mapper: F) -> Self {
        Self {
            grid: &grid.inner,
            mapper,
        }
    }
}

impl<T, O, F> fmt::Display for GridPrinter<'_, T, F>
where
    F: Fn(&T) -> O,
    O: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first_row = true;
        for row in 0..self.grid.rows() {
            if !first_row {
                writeln!(f)?;
            }

            for col in 0..self.grid.cols() {
                let val = (self.mapper)(self.grid.get(row, col).unwrap());
                write!(f, "{}", val)?;
            }

            first_row = false;
        }
        Ok(())
    }
}
