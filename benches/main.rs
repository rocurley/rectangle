#[macro_use]
extern crate criterion;
extern crate cpuprofiler;
extern crate typed_arena;
use cpuprofiler::PROFILER;
use typed_arena::Arena;
extern crate ascii;
use ascii::{AsAsciiStr, AsciiChar, AsciiStr};
extern crate ndarray;
use criterion::{BenchmarkId, Criterion};
use ndarray::Array;
use std::collections::HashSet;
extern crate rectangle;
use rectangle::{
    load_words, prepopulate_cache, step_word_rectangle, CrushedWords, WordRectangle, WordsMatch,
};

fn criterion_benchmark(c: &mut Criterion) {
    let words_path = "/usr/share/dict/words";
    const DIM: usize = 15;
    let words_by_length = load_words(words_path, DIM, Some(DIM + 1));
    let fixed_matches_slab: Arena<Vec<&[AsciiChar]>> = Arena::new();
    let fixed_possible_chars_slab: Arena<Vec<HashSet<AsciiChar>>> = Arena::new();
    let caches = prepopulate_cache(
        &fixed_matches_slab,
        &fixed_possible_chars_slab,
        &words_by_length,
    );
    let words_by_length_borrowed = words_by_length
        .iter()
        .map(|(&l, words)| (l, words.borrow()))
        .collect();
    let alphabet_string: &AsciiStr = "abcdefghijklmnopqrstuvwxyz"
        .as_ascii_str()
        .expect("Couldn't construct alphabet");
    let alphabet: HashSet<AsciiChar> = alphabet_string.chars().copied().collect();
    let empty = Array::from_elem((DIM, DIM), alphabet.clone());
    c.bench_with_input(
        BenchmarkId::new("step_word_rectangle", format!("{}x{}", DIM, DIM)),
        &(words_by_length_borrowed, caches),
        |b, (words_by_length_borrowed, caches)| {
            b.iter(|| {
                let mut local_caches = caches.clone();
                let local_matches_slab: Arena<Vec<&[AsciiChar]>> = Arena::new();
                let local_possible_chars_slab: Arena<Vec<HashSet<AsciiChar>>> = Arena::new();
                let mut row_matches = Vec::new();
                for _ in 0..DIM {
                    row_matches.push(WordsMatch::Unconstrained)
                }
                let mut col_matches = Vec::new();
                for _ in 0..DIM {
                    col_matches.push(WordsMatch::Unconstrained)
                }
                let start = WordRectangle {
                    array: empty.clone(),
                    row_matches: row_matches,
                    col_matches: col_matches,
                };
                step_word_rectangle(
                    &words_by_length_borrowed,
                    &local_matches_slab,
                    &local_possible_chars_slab,
                    &mut local_caches,
                    start,
                    false,
                )
            })
        },
    );
    PROFILER.lock().unwrap().stop().unwrap();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
