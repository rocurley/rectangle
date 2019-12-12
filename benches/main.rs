#[macro_use]
extern crate criterion;
extern crate typed_arena;
use typed_arena::Arena;
extern crate ascii;
use ascii::AsciiChar;
extern crate ndarray;
use ndarray::Array;

use criterion::black_box;
use criterion::{BenchmarkId, Criterion};

extern crate rectangle;
use rectangle::{preprocess_words, step_word_rectangle, WordRectangle, WordsMatch};

fn criterion_benchmark(c: &mut Criterion) {
    let mut words = Vec::new();
    let fixed_slab: Arena<Vec<&[AsciiChar]>> = Arena::new();
    let words_path = "/usr/share/dict/words";
    const DIM: usize = 15;
    let (words_by_length, caches) =
        preprocess_words(&mut words, &fixed_slab, words_path, DIM, Some(DIM + 1));
    c.bench_with_input(
        BenchmarkId::new("step_word_rectangle", format!("{}x{}", DIM, DIM)),
        &(words_by_length, caches),
        |b, (words_by_length, caches)| {
            b.iter(|| {
                let mut local_caches = caches.clone();
                let local_slab: Arena<Vec<&[AsciiChar]>> = Arena::new();
                let empty = Array::from_elem((DIM, DIM), None);
                let mut row_matches = Vec::new();
                for _ in 0..DIM {
                    row_matches.push(WordsMatch::Unconstrained)
                }
                let mut col_matches = Vec::new();
                for _ in 0..DIM {
                    col_matches.push(WordsMatch::Unconstrained)
                }
                let start = WordRectangle {
                    array: empty,
                    row_matches: row_matches,
                    col_matches: col_matches,
                };
                step_word_rectangle(
                    &words_by_length,
                    &local_slab,
                    &mut local_caches,
                    start,
                    false,
                )
            })
        },
    );
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
