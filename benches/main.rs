#[macro_use]
extern crate criterion;
extern crate cpuprofiler;
use cpuprofiler::PROFILER;
use criterion::{BenchmarkId, Criterion};
extern crate rectangle;
use rectangle::{load_words, prepopulate_cache, step_word_rectangle, WordRectangle, WordsMatch};

fn criterion_benchmark(c: &mut Criterion) {
    let words_path = "/usr/share/dict/words";
    const DIM: usize = 15;
    let words_by_length = load_words(words_path, DIM, Some(DIM + 1));
    let caches = prepopulate_cache(&words_by_length);
    c.bench_with_input(
        BenchmarkId::new("step_word_rectangle", format!("{}x{}", DIM, DIM)),
        &caches,
        |b, caches| {
            b.iter(|| {
                let mut row_matches = Vec::new();
                for _ in 0..DIM {
                    row_matches.push(WordsMatch::Unconstrained)
                }
                let mut col_matches = Vec::new();
                for _ in 0..DIM {
                    col_matches.push(WordsMatch::Unconstrained)
                }
                let start = WordRectangle {
                    row_matches: row_matches,
                    row_cache: &caches[&DIM],
                    col_cache: &caches[&DIM],
                    col_matches: col_matches,
                };
                step_word_rectangle(start, false, 0)
            })
        },
    );
    PROFILER.lock().unwrap().stop().unwrap();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
