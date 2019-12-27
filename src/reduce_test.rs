use {load_words, prepopulate_cache, WordRectangle};

#[test]
fn test_reduce() {
    let words = load_words("testdata/short_words", 0, None);
    let caches = prepopulate_cache(&words);
    let start = WordRectangle::new(2, 2, &caches);
    let reduced = start.reduce().expect("Reduce eliminated rectangle");
    assert_eq!("as\nso", reduced.show());
}
