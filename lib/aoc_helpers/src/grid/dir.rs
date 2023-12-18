use std::ops::{Add, AddAssign, Mul, Sub, SubAssign};

use glam::IVec2;

pub type Pos = IVec2;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Dir {
    N,
    E,
    S,
    W,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Side {
    Left,
    Right,
}

impl Dir {
    #[inline]
    pub const fn all() -> [Dir; 4] {
        [Dir::N, Dir::E, Dir::S, Dir::W]
    }

    #[inline]
    pub const fn opposite(&self) -> Dir {
        match self {
            Dir::N => Dir::S,
            Dir::E => Dir::W,
            Dir::S => Dir::N,
            Dir::W => Dir::E,
        }
    }

    #[inline]
    pub const fn to_vec(&self) -> IVec2 {
        match self {
            Dir::N => IVec2::NEG_Y,
            Dir::E => IVec2::X,
            Dir::S => IVec2::Y,
            Dir::W => IVec2::NEG_X,
        }
    }

    #[inline]
    pub const fn from_vec(vec: IVec2) -> Option<Self> {
        match (vec.x, vec.y) {
            (0, -1) => Some(Dir::N),
            (1, 0) => Some(Dir::E),
            (0, 1) => Some(Dir::S),
            (-1, 0) => Some(Dir::W),
            _ => None,
        }
    }

    #[inline]
    pub const fn parallel_to(vec: IVec2) -> Option<Self> {
        let IVec2 { mut x, mut y } = vec;
        x = x.signum();
        y = y.signum();
        Dir::from_vec(IVec2 { x, y })
    }

    #[inline]
    pub const fn from_a_to_b(a: Pos, b: Pos) -> Option<Self> {
        let Pos { x: ax, y: ay } = a;
        let Pos { x: bx, y: by } = b;

        let diff = Pos {
            x: bx - ax,
            y: by - ay,
        };
        Dir::parallel_to(diff)
    }

    /// Returns an iterator that yields positions `pos + self`, `pos + self +
    /// self`, `...`.
    #[inline]
    pub fn advance(&self, start_pos: Pos) -> Advance {
        Advance::new(start_pos, *self)
    }

    #[inline]
    pub const fn turn_to(&self, other: Dir) -> Option<Side> {
        use Dir::*;
        let side = match (*self, other) {
            (N, E) => Side::Right,
            (N, W) => Side::Left,

            (E, S) => Side::Right,
            (E, N) => Side::Left,

            (S, W) => Side::Right,
            (S, E) => Side::Left,

            (W, N) => Side::Right,
            (W, S) => Side::Left,

            _ => return None,
        };
        Some(side)
    }

    #[inline]
    pub const fn turn(&self, side: Side) -> Dir {
        use Dir::*;
        use Side::*;

        match (*self, side) {
            (N, Left) => W,
            (N, Right) => E,

            (E, Left) => N,
            (E, Right) => S,

            (S, Left) => E,
            (S, Right) => W,

            (W, Left) => S,
            (W, Right) => N,
        }
    }
}

impl Add<Dir> for Pos {
    type Output = Pos;

    #[inline]
    fn add(self, rhs: Dir) -> Self::Output {
        self + rhs.to_vec()
    }
}

impl AddAssign<Dir> for Pos {
    #[inline]
    fn add_assign(&mut self, rhs: Dir) {
        *self += rhs.to_vec()
    }
}

impl Sub<Dir> for Pos {
    type Output = Pos;

    #[inline]
    fn sub(self, rhs: Dir) -> Self::Output {
        self - rhs.to_vec()
    }
}

impl SubAssign<Dir> for Pos {
    fn sub_assign(&mut self, rhs: Dir) {
        *self -= rhs.to_vec()
    }
}

impl Mul<i32> for Dir {
    type Output = Pos;

    #[inline]
    fn mul(self, rhs: i32) -> Self::Output {
        self.to_vec() * rhs
    }
}

impl Mul<Dir> for i32 {
    type Output = Pos;

    #[inline]
    fn mul(self, rhs: Dir) -> Self::Output {
        rhs.to_vec() * self
    }
}

pub struct Advance {
    pos: Pos,
    dir: Dir,
}

impl Advance {
    pub fn new(start_pos: Pos, dir: Dir) -> Self {
        Self {
            pos: start_pos,
            dir,
        }
    }
}

impl Iterator for Advance {
    type Item = Pos;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.pos + self.dir;

        self.pos = next;

        Some(next)
    }
}
