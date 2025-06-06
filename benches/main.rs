#[macro_use]
extern crate criterion;
extern crate cpuprofiler;
extern crate typed_arena;
use std::collections::HashMap;

use cpuprofiler::PROFILER;
extern crate ascii;
extern crate ndarray;

use criterion::{BenchmarkId, Criterion};

extern crate rectangle;
use rectangle::{load_words, prefix::PrefixTree, WordRectangle};

fn criterion_benchmark(c: &mut Criterion) {
    let words_path = "/usr/share/dict/words";
    const DIM: usize = 15;
    let words_by_length = load_words(words_path, DIM, Some(DIM + 1));
    let indices: HashMap<_, _> = words_by_length
        .iter()
        .map(|(k, v)| (*k, PrefixTree::new(v.borrow())))
        .collect();
    c.bench_with_input(
        BenchmarkId::new("step_word_rectangle", format!("{}x{}", DIM, DIM)),
        &indices,
        |b, indices| {
            b.iter(|| {
                let start = WordRectangle::new(DIM, DIM, &indices);
                start.solve()
            })
        },
    );
    // PROFILER.lock().unwrap().stop().unwrap();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
