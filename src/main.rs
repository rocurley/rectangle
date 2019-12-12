extern crate ascii;
use ascii::AsciiChar;

use std::time::Instant;

extern crate ndarray;
use ndarray::Array;

extern crate cpuprofiler;
use cpuprofiler::PROFILER;

extern crate typed_arena;
use typed_arena::Arena;

#[macro_use]
extern crate clap;

extern crate rectangle;
use rectangle::{
    preprocess_words, show_word_rectangle, step_word_rectangle, WordRectangle, WordsMatch::*,
};

fn main() {
    let config = clap_app!(rectangle =>
      (@arg WORDS: +required "File to pull words from")
      (@arg skip: --skip +takes_value "Skip areas greater than this parameter")
      (@arg min_len: --min_len + takes_value "Minimum word length")
      (@arg max_len: --max_len + takes_value "Maximum word length")
    )
    .get_matches();
    let words_path = config.value_of("WORDS").expect("No words file");
    let skip: Option<usize> = config
        .value_of("skip")
        .map(|s| s.parse().expect("Could not parse skip"));
    let min_len = config
        .value_of("min_len")
        .map_or(0, |s| s.parse().expect("Could not parse min_len"));
    let max_len = config
        .value_of("max_len")
        .map(|s| s.parse().expect("Could not parse max_len"));
    let max_len = match (max_len, skip) {
        (Some(l), Some(r)) => Some(std::cmp::max(l, r)),
        (x, None) => x,
        (None, x) => x,
    };
    let slab: Arena<Vec<&[AsciiChar]>> = Arena::new();
    let mut words = Vec::new();
    let (words_by_length, mut caches) =
        preprocess_words(&mut words, &slab, words_path, min_len, max_len);
    let mut dims = Vec::new();
    for x in words_by_length.keys() {
        for y in words_by_length.keys() {
            if (x >= y) && skip.map_or(true, |s| x * y < s) {
                dims.push((x, y));
            }
        }
    }
    dims.sort_by_key(|&(x, y)| -((x * y) as i64));
    for &(&w, &h) in dims.iter() {
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
        println!("{}x{}", w, h);
        PROFILER
            .lock()
            .unwrap()
            .start(format!("profiling/{}x{}.profile", w, h))
            .unwrap();
        let start_time = Instant::now();
        match step_word_rectangle(&words_by_length, &slab, &mut caches, start, true) {
            None => println!("No rectangle found"),
            Some(rect) => println!("Found:\n{}", show_word_rectangle(&rect)),
        }
        let elapsed = start_time.elapsed();
        println!("{:?}", elapsed);
        PROFILER.lock().unwrap().stop().unwrap();
    }
}
