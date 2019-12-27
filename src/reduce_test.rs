use {load_words, prepopulate_cache, Alpha, WordRectangle};

#[test]
fn test_reduce_2x2() {
    let words = load_words("testdata/short_words", 0, None);
    let caches = prepopulate_cache(&words);
    let start = WordRectangle::new(2, 2, &caches);
    let reduced = start.reduce().expect("Reduce eliminated rectangle");
    assert_eq!("as\nso", reduced.show());
}
#[test]
fn test_reduce_2x2_fail() {
    let mut words = load_words("testdata/short_words", 0, None);
    words
        .get_mut(&2)
        .unwrap()
        .push(&Alpha::from_str("xx").unwrap());
    let caches = prepopulate_cache(&words);
    let start = WordRectangle::new(2, 2, &caches);
    dbg!(&start.row_cache);
    let reduced = start.reduce().expect("Reduce eliminated rectangle");
    assert_eq!("??\n??", reduced.show());
}

#[test]
fn test_reduce_4x4() {
    let words = load_words("testdata/short_words", 0, None);
    let caches = prepopulate_cache(&words);
    let start = WordRectangle::new(4, 4, &caches);
    let reduced = start.reduce().expect("Reduce eliminated rectangle");
    assert_eq!("abed\nbeau\nearl\ndull", reduced.show());
}
