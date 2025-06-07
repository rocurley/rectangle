use std::{collections::HashMap, time::Instant};

use clap::clap_app;
use rectangle::{load_words, prefix::PrefixTree, word_rectangle::WordRectangle};

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
    let indices: HashMap<_, _> = words_by_length
        .iter()
        .map(|(k, v)| (*k, PrefixTree::new(v.borrow())))
        .collect();
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
        let start = WordRectangle::new(w, h, &indices);
        print!("{}x{}:\t", w, h);
        let (solution, stats) = start.solve();
        print!("{:4.4}\t{:10}\t", stats.runtime.as_secs_f64(), stats.calls);
        match solution {
            None => println!("no rectangle found "),
            Some(rect) => println!("Found:\n{}", rect.show()),
        }
    }
}
