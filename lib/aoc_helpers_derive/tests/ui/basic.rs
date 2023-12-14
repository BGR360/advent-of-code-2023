use aoc_helpers_derive::Tile;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Tile)]
enum MyTile {
    #[default]
    #[tile('.')]
    Empty,
    #[tile('#')]
    Full,
}

fn main() {
    let e = MyTile::Empty;
    let f = MyTile::Full;

    assert_eq!(e.symbol(), b'.');
    assert_eq!(f.symbol(), b'#');

    assert_eq!(u8::from(e), b'.');
    assert_eq!(u8::from(f), b'#');

    assert_eq!(char::from(e), '.');
    assert_eq!(char::from(f), '#');

    assert_eq!(MyTile::try_from(b'.'), Ok(e));
    assert_eq!(MyTile::try_from(b'#'), Ok(f));

    assert_eq!(MyTile::try_from('.'), Ok(e));
    assert_eq!(MyTile::try_from('#'), Ok(f));

    assert_eq!(".".parse(), Ok(e));
    assert_eq!("#".parse(), Ok(f));

    assert_eq!(&format!("{e} {f}"), ". #");

    let mut parse_both = nom::sequence::tuple((MyTile::parse, MyTile::parse));
    let (rest, (t1, t2)) = parse_both(".#abcd").unwrap();
    assert_eq!(rest, "abcd");
    assert_eq!(t1, e);
    assert_eq!(t2, f);
}
