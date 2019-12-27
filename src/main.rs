use std::time::Instant;
extern crate cpuprofiler;
use cpuprofiler::PROFILER;
#[macro_use]
extern crate clap;
extern crate rectangle;
use rectangle::{load_words, prepopulate_cache, step_word_rectangle, WordRectangle, WordsMatch::*};

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
    let words_by_length = load_words(words_path, min_len, max_len);
    let caches = prepopulate_cache(&words_by_length);
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
        let start = WordRectangle::new(w, h, &caches);
        println!("{}x{}", w, h);
        PROFILER
            .lock()
            .unwrap()
            .start(format!("profiling/{}x{}.profile", w, h))
            .unwrap();
        let start_time = Instant::now();
        match step_word_rectangle(start, true) {
            (None, calls) => println!("No rectangle found in {} calls", calls),
            (Some(rect), calls) => println!("Found in {} calls:\n{}", calls, rect.show()),
        }
        let elapsed = start_time.elapsed();
        println!("{:?}", elapsed);
        PROFILER.lock().unwrap().stop().unwrap();
    }
}
