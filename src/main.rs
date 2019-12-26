extern crate enumset;
use enumset::EnumSet;
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
    load_words, prepopulate_cache, show_word_rectangle, step_word_rectangle, Alpha, WordRectangle,
    WordsMatch::*,
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
    let matches_slab: Arena<Vec<&[Alpha]>> = Arena::new();
    let possible_chars_slab: Arena<Vec<EnumSet<Alpha>>> = Arena::new();
    let words_by_length = load_words(words_path, min_len, max_len);
    let mut caches = prepopulate_cache(&matches_slab, &possible_chars_slab, &words_by_length);
    let mut dims = Vec::new();
    for x in words_by_length.keys() {
        for y in words_by_length.keys() {
            if (x >= y) && skip.map_or(true, |s| x * y < s) {
                dims.push((x, y));
            }
        }
    }
    let words_by_length_borrowed = words_by_length
        .iter()
        .map(|(&l, words)| (l, words.borrow()))
        .collect();
    dims.sort_by_key(|&(x, y)| -((x * y) as i64));
    let alphabet_string = "abcdefghijklmnopqrstuvwxyz";
    let alphabet: EnumSet<Alpha> = alphabet_string
        .chars()
        .map(Alpha::from_char)
        .collect::<Option<EnumSet<Alpha>>>()
        .expect("Somehow not alpha");
    for &(&w, &h) in dims.iter() {
        let empty = Array::from_elem((h, w), alphabet.clone());
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
            row_matches,
            col_matches,
        };
        println!("{}x{}", w, h);
        PROFILER
            .lock()
            .unwrap()
            .start(format!("profiling/{}x{}.profile", w, h))
            .unwrap();
        let start_time = Instant::now();
        match step_word_rectangle(
            &words_by_length_borrowed,
            &matches_slab,
            &possible_chars_slab,
            &mut caches,
            start,
            true,
        ) {
            (None, calls) => println!("No rectangle found in {} calls", calls),
            (Some(rect), calls) => {
                println!("Found in {} calls:\n{}", calls, show_word_rectangle(&rect))
            }
        }
        let elapsed = start_time.elapsed();
        println!("{:?}", elapsed);
        PROFILER.lock().unwrap().stop().unwrap();
    }
}
