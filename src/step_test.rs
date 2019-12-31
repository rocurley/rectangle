extern crate proptest;
use self::proptest::prelude::*;
use self::proptest::sample::subsequence;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use {
    load_words, prepopulate_cache, step_word_rectangle, Alpha, Counters, CrushedWords,
    WordRectangle,
};
#[test]
fn test_step_2x2_multiple() {
    let words = load_words("testdata/evil_words", 0, None);
    let caches = prepopulate_cache(&words);
    let start = WordRectangle::new(2, 2, &caches);
    let res = step_word_rectangle(start, false, 0, &mut Counters::new());
    let res = res.expect("Reduce eliminated rectangle");
    match res.show().as_str() {
        "la\nay" => {}
        "ma\nay" => {}
        other => panic!("Expected asso or xxxx, got {}", other),
    }
}

fn words_strategy(words_path: &str, len: usize) -> impl Strategy<Value = Vec<Vec<Alpha>>> {
    let f = File::open(words_path).expect("Could not open file");
    let file = BufReader::new(&f);
    let words: Vec<Vec<Alpha>> = file
        .lines()
        .map(|line| line.expect("Not a line or something"))
        .filter(|word| word.chars().all(|c| c.is_ascii_lowercase()) && word.len() == len)
        .map(|word| Alpha::from_str(&word).expect("Somehow not ascii"))
        .collect();
    let range = 1..words.len();
    subsequence(words, range)
}

proptest! {
    #[test]
    fn test_prop(words in words_strategy("/usr/share/dict/words", 2)) {
        let crushed :CrushedWords= words.iter().map(Vec::as_slice).collect();
        let mut words_by_length = HashMap::new();
        words_by_length.insert(2, crushed);
        let caches = prepopulate_cache(&words_by_length);
        let start = WordRectangle::new(2, 2, &caches);
        step_word_rectangle(start, false, 0, &mut Counters::new());
    }
}
