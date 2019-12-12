#[macro_use]
extern crate criterion;
extern crate typed_arena;
use typed_arena::Arena;
extern crate ascii;
use ascii::{AsciiChar, AsciiString};

use criterion::black_box;
use criterion::Criterion;

fn criterion_benchmark(c: &mut Criterion) {
    let fixed_slab: Arena<Vec<&[AsciiChar]>> = Arena::new();
    let mut words = Vec::new();
    let (words_by_length, mut caches) =
        preprocess_words(&mut words, &fixed_slab, words_path, min_len, max_len);
    c.bench_function("12x12", |b| {
        b.iter(|| {
            let local_slab: Arena<Vec<&[AsciiChar]>> = Arena::new();
            let h = 12;
            let w = 12;
            let empty = Array::from_elem((h, w), None);
            let mut row_matches = Vec::new();
            for _ in 0..h {
                row_matches.push(Unconstrained)
            }
            let mut col_matches = Vec::new();
            for _ in 0..w {
                col_matches.push(Unconstrained)
            }
            let start = WordRectangle {
                array: empty,
                row_matches: row_matches,
                col_matches: col_matches,
            };
            step_word_rectangle(&words_by_length, &slab, &mut caches, start, true)
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
