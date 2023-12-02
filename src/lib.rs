mod day;
pub mod helpers;
pub mod template;

pub use day::*;

#[macro_export]
macro_rules! debug {
    ($($args:expr),*) => {{
        #[cfg(debug_assertions)]
        print!($($args),*);
    }};
}

#[macro_export]
macro_rules! debugln {
    ($($args:expr),*) => {{
        #[cfg(debug_assertions)]
        println!($($args),*);
    }};
}
