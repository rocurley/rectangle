#[macro_use]
extern crate criterion;
extern crate cpuprofiler;
use cpuprofiler::PROFILER;
use criterion::{BenchmarkId, Criterion};
extern crate rectangle;
use rectangle::{load_words, prepopulate_cache, step_word_rectangle, WordRectangle, WordsMatch};
extern crate rand;
use rand::{rngs::StdRng, Rng, SeedableRng};
extern crate bit_vec;
use bit_vec::BitVec;
extern crate bit_set;
use bit_set::BitSet;

fn step_benchmark(c: &mut Criterion) {
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

fn bitset_native_intersection(c: &mut Criterion) {
    let mut rng: StdRng = SeedableRng::seed_from_u64(0);
    let mut vec1: Vec<u8> = Vec::new();
    let mut vec2: Vec<u8> = Vec::new();
    for _ in (1..1000) {
        vec1.push(rng.gen());
        vec2.push(rng.gen());
    }
    let bitvec1 = BitVec::from_bytes(&vec1);
    let bitvec2 = BitVec::from_bytes(&vec2);
    let mut bitset1 = BitSet::from_bit_vec(bitvec1);
    let bitset2 = BitSet::from_bit_vec(bitvec2);
    c.bench_function("bitset_native_intersection", |b| {
        b.iter(|| {
            bitset1.intersect_with(&bitset2);
        })
    });
}

pub fn bitwise_and(left: &mut [u8], right: &[u8]) {
    for (l, r) in left.iter_mut().zip(right) {
        *l &= r;
    }
}

fn bitset_optimized_intersection(c: &mut Criterion) {
    let mut rng: StdRng = SeedableRng::seed_from_u64(0);
    let mut vec1: Vec<u8> = Vec::new();
    let mut vec2: Vec<u8> = Vec::new();
    for _ in (1..1000) {
        vec1.push(rng.gen());
        vec2.push(rng.gen());
    }
    c.bench_function("bitset_optimized_intersection", |b| {
        b.iter(|| {
            bitwise_and(&mut vec1, &vec2);
        })
    });
}

criterion_group!(
    benches,
    //step_benchmark,
    bitset_native_intersection,
    bitset_optimized_intersection,
);
criterion_main!(benches);
