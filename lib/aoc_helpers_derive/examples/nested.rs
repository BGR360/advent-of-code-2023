use aoc_helpers_derive::Tile;

#[derive(Tile)]
enum Outer {
    #[tile('a')]
    A,
    #[tile('b')]
    B,
    Other(Inner),
}

#[derive(Tile)]
enum Inner {
    #[tile('x')]
    X,
    #[tile('y')]
    Y,
}

fn main() {}
