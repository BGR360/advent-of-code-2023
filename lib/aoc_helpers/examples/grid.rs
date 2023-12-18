use aoc_helpers::{grid, parsing};

#[derive(Debug, grid::Tile)]
enum Tile {
    #[tile('?')]
    Unknown,
    Known(Letter),
}
#[derive(Debug, grid::Tile)]
enum Letter {
    #[tile('A')] A,
    #[tile('B')] B,
}
fn main() {
    let input =
"A?B\n\
BBA\n\
A??\n";

    let (_, tiles): (&str, grid::Grid<Tile>) =
        parsing::grid(Tile::parse)(input).unwrap();

    println!("{tiles}");
    for (pos, tile) in tiles.indexed_iter() {
        println!("{pos}: {tile:?}");
    }
}
