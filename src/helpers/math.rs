pub use glam::{IVec2, UVec2};

pub trait CheckedAdd<Rhs = Self> {
    type Output;

    fn checked_add(self, rhs: Rhs) -> Option<Self::Output>;
}

pub trait CheckedSub<Rhs = Self> {
    type Output;

    fn checked_sub(self, rhs: Rhs) -> Option<Self::Output>;
}

macro_rules! impl_checked_trait_for_glam_vec {
    ($trait:ident, $method:ident, $ty:ident) => {
        impl $trait for $ty {
            type Output = Self;

            #[inline(always)]
            fn $method(self, rhs: Self) -> Option<Self::Output> {
                Some(Self {
                    x: self.x.$method(rhs.x)?,
                    y: self.y.$method(rhs.y)?,
                })
            }
        }
    };
}

impl_checked_trait_for_glam_vec!(CheckedAdd, checked_add, UVec2);
impl_checked_trait_for_glam_vec!(CheckedSub, checked_sub, UVec2);

impl_checked_trait_for_glam_vec!(CheckedAdd, checked_add, IVec2);
impl_checked_trait_for_glam_vec!(CheckedSub, checked_sub, IVec2);
