#![feature(conservative_impl_trait)]
#![feature(ascii_ctype)]

use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;
use std::ascii::AsciiExt;

extern crate ascii;
use ascii::{AsciiString, AsciiChar};

use std::collections::HashMap;

use std::cmp::Ordering;

use std::time::Instant;

extern crate itertools;
use itertools::join;

extern crate ndarray;
use ndarray::{Array, Array2, Axis, Zip};

extern crate pbr;
use pbr::ProgressBar;

extern crate cpuprofiler;
use cpuprofiler::PROFILER;

#[macro_use]
extern crate clap;

fn main() {
  let config = clap_app!(rectangle =>
    (@arg WORDS: +required "File to pull words from")
    (@arg skip: --skip +takes_value "Skip areas greater than this parameter")
    (@arg min_len: --min_len + takes_value "Minimum word length")
    (@arg max_len: --max_len + takes_value "Maximum word length")
  ).get_matches();
  let words_path = config.value_of("WORDS").expect("No words file");
  let skip : Option<usize> = config.value_of("skip").map(|s| s.parse().expect("Could not parse skip"));
  let min_len = config.value_of("min_len").map_or(0, |s| s.parse().expect("Could not parse min_len"));
  let max_len = config.value_of("max_len").map(|s| s.parse().expect("Could not parse max_len"));
  let max_len = match (max_len, skip) {
    (Some(l), Some(r)) => Some(std::cmp::max(l,r)),
    (x, None) => x,
    (None, x) => x,
  };
  let f = File::open(words_path).expect("Could not open file");
  let file = BufReader::new(&f);
  let words : Vec<AsciiString>;
  let mut words_by_length : HashMap<usize, Vec<& [AsciiChar]>> = HashMap::new();
  let mut indices : HashMap<usize, Vec<Vec<& [AsciiChar]>>> = HashMap::new();
  words = file.lines()
    .map(|line| line.expect("Not a line or something"))
    .filter(|word|
      word.is_ascii_lowercase() &&
      word.len() >= min_len &&
      max_len.map_or(true, |max| word.len() < max)
    ).map(|word| AsciiString::from_ascii(word).expect("Somehow not ascii"))
    .collect();
  let mut words_pb = ProgressBar::new(words.len() as u64);
  println!("Preprocessing words");
  for word in words.iter() {
    let l = word.len();

    let same_length = words_by_length.entry(l).or_insert(Vec::new());
    same_length.push(word.as_slice());

    let index = indices.entry(l).or_insert(vec![Vec::new(); 26*l]);
    for (pos, &c) in word.chars().enumerate() {
      index[ix(pos, c)].push(word.as_slice());
    }

    words_pb.inc();
  };
  words_pb.finish_print("Preprocessing complete");
  for (&k, index) in indices.iter() {
    println!("{}: {}", k, words_by_length[&k].len());
    for char_index in index.iter(){
      for word in char_index.iter() {
        assert!(word.len() == k, "{:?} found in index for {}", word, k);
      }
    }
  }
  let mut dims = Vec::new();
  for x in words_by_length.keys() {
    for y in words_by_length.keys() {
      if (x >= y) && skip.map_or(true, |s| x*y < s) {
        dims.push((x,y));
      }
    }
  };
  dims.sort_by_key(|&(x,y)| -((x*y) as i64));
  for &(&w,&h) in dims.iter(){
    let empty = Array::from_elem((h,w), None);
    let mut row_matches = Vec::new();
    for _ in 0..h {row_matches.push(Unconstrained)};
    let mut col_matches = Vec::new();
    for _ in 0..w {col_matches.push(Unconstrained)};
    let start = WordRectangle {
      array: empty,
      row_matches : row_matches,
      col_matches : col_matches,
    };
    println!("{}x{}", w, h);
    //let mut stack = vec![empty];
    //let mut pb_tuple : Option<(ProgressBar<_>, u64)> = None;
    PROFILER.lock().unwrap().start(format!("profiling/{}x{}.profile", w,h)).unwrap();
    let start_time = Instant::now();
    match step_word_rectangle(& indices, & words_by_length, start, true) {
      None => println!("No rectangle found"),
      Some(rect) => println!("Found:\n{}", show_word_rectangle(& rect)),
    }
    let elapsed = start_time.elapsed();
    println!("{:?}", elapsed);
    PROFILER.lock().unwrap().stop().unwrap();
  };
}

fn ix(pos : usize, c : AsciiChar) -> usize {
  return pos*26+(c as usize - 'a' as usize)
}

fn unstable_retain<F,A>(v : &mut Vec<A>, mut f: F) where F: FnMut(&A) -> bool {
  let len = v.len();
  let mut kept = 0;
  let mut i = len-1;
  {
    let s = v.as_mut_slice();
    if len == 0 {
      return;
    }

    while i >= kept && i < len {
      if f(&s[i]) {
        s.swap(i, kept);
        kept += 1;
      } else {
        i -= 1;
      };
    }
  }
  v.truncate(kept);
}

#[derive(Debug)]
enum WordsMatch <'a, 'w : 'a>{
  Unconstrained,
  Filled,
  OwnedMatches { matches: Vec<& 'w[AsciiChar]>},
  BorrowedMatches { matches: & 'a Vec<& 'w[AsciiChar]>},
}
use WordsMatch::*;

impl <'a, 'w : 'a> WordsMatch<'a, 'w> {
  fn shallow_copy<'b>(& 'b self) -> WordsMatch<'b, 'w> {
    match *self {
      Unconstrained => Unconstrained,
      Filled => Filled,
      OwnedMatches{ ref matches} => BorrowedMatches {matches},
      BorrowedMatches{ matches } => BorrowedMatches {matches},
    }
  }

  fn apply_constraint<'b : 'a>(& mut self, index : &'b Vec<Vec<& 'w[AsciiChar]>>
    , pos : usize
    , c : AsciiChar) {
    match *self {
      Unconstrained => *self = BorrowedMatches{matches: & index[ix(pos, c)]},
      OwnedMatches {ref mut matches} => {
        unstable_retain(matches, |word| word[pos] == c);
      },
      BorrowedMatches { matches: old_matches} => {
        let mut new_matches = old_matches.clone();
        unstable_retain(& mut new_matches, |word| word[pos] == c);
        *self = OwnedMatches { matches : new_matches}
      },
      Filled => {}
    }
  }
}

impl<'a, 'w> Ord for WordsMatch<'a, 'w> {
  fn cmp(&self, other: & WordsMatch) -> Ordering {
    match(self, other) {
      (& Filled, & Filled) => Ordering::Equal,
      (& Filled, _) => Ordering::Greater,
      (_, & Filled) => Ordering::Less,
      (& Unconstrained, & Unconstrained) => Ordering::Equal,
      (& Unconstrained, _) => Ordering::Greater,
      (_, & Unconstrained) => Ordering::Less,
      (& OwnedMatches{matches: ref l}, & OwnedMatches{matches: ref r}) => l.len().cmp(& r.len()),
      (& BorrowedMatches{matches: l}, & OwnedMatches{matches: ref r}) => l.len().cmp(& r.len()),
      (& OwnedMatches{matches: ref l}, & BorrowedMatches{matches: r}) => l.len().cmp(& r.len()),
      (& BorrowedMatches{matches: l}, & BorrowedMatches{matches: r}) => l.len().cmp(& r.len()),
    }
  }
}

impl<'a, 'w> PartialOrd for WordsMatch<'a, 'w> {
  fn partial_cmp(&self, other: &WordsMatch) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a, 'w> PartialEq for WordsMatch<'a, 'w> {
  fn eq(&self, other: & WordsMatch) -> bool {
    match self.cmp(other) {
      Ordering::Equal => true,
      _ => false,
    }
  }
}

impl<'a, 'w> Eq for WordsMatch<'a, 'w> {}

#[derive(Debug)]
struct WordRectangle<'a, 'w : 'a> {
  array : Array2<Option<AsciiChar>>,
  row_matches : Vec<WordsMatch<'a, 'w>>,
  col_matches : Vec<WordsMatch<'a, 'w>>,
}

impl <'a, 'w> WordRectangle<'a, 'w> {
  fn shallow_copy<'b>(& 'b self) -> WordRectangle<'b, 'w>{
    WordRectangle{
      array : self.array.clone(),
      row_matches : self.row_matches.iter().map(|m| m.shallow_copy()).collect(),
      col_matches : self.col_matches.iter().map(|m| m.shallow_copy()).collect(),
    }
  }
}

fn show_word_rectangle(word_rectangle : & Array2<Option<AsciiChar>>) -> String {
  let rows = word_rectangle.outer_iter().map(|row|
    row.iter().map(|c| c.map_or('.',|c| c.as_char())).collect::<String>()
  );
  join(rows,"\n")
}

fn step_word_rectangle<'a, 'w>(
  indices : & HashMap<usize, Vec<Vec<& 'w[AsciiChar]>>>,
  words_by_length : & 'a HashMap<usize, Vec<& 'w[AsciiChar]>>,
  word_rectangle : WordRectangle<'a, 'w>,
  show_pb : bool,
  ) -> Option<Array2<Option<AsciiChar>>>
  {
  let width = word_rectangle.array.shape()[1];
  let height = word_rectangle.array.shape()[0];
  let unfiltered_row_candidates = &words_by_length[&width];
  let unfiltered_col_candidates = &words_by_length[&height];

  let (best_row_ix, best_row_matches) =
    word_rectangle.row_matches.iter().enumerate()
      .min_by(|& (_, ref l_matches), & (_, ref r_matches)| l_matches.cmp(r_matches)).expect("Empty rows");
  let (best_col_ix, best_col_matches) =
    word_rectangle.col_matches.iter().enumerate()
      .min_by(|& (_, ref l_matches), & (_, ref r_matches)| l_matches.cmp(r_matches)).expect("Empty cols");
  if (& best_row_matches, unfiltered_row_candidates.len()) <
    (& best_col_matches, unfiltered_col_candidates.len()) {
      let matches : & [& [AsciiChar]] = match *best_row_matches {
        Filled => return Some(word_rectangle.array),
        Unconstrained => unfiltered_row_candidates,
        OwnedMatches{matches:ref m} => m,
        BorrowedMatches{matches:m} => m,
      };
      let mut pb = if show_pb {
        Some(ProgressBar::new(matches.len() as u64))
      } else {
        None
      };
      for & word in matches {
        for p in & mut pb {p.inc();};
        let mut new_rectangle = word_rectangle.shallow_copy();
        new_rectangle.row_matches[best_row_ix] = Filled;
        {
          let target = new_rectangle.array.subview_mut(Axis(0),best_row_ix);
          Zip::from(word)
            .and(target)
            .apply(|& word_c, target_slot| *target_slot = Some(word_c));
          Zip::from(word)
            .and(& mut new_rectangle.col_matches)
            .apply(|&c, col| col.apply_constraint( &indices[&height], best_row_ix, c));
        }
        let child_result = step_word_rectangle(indices, words_by_length, new_rectangle, false);
        if child_result.is_some() {
          return child_result;
        }
      }
  } else {
      let matches : & [& [AsciiChar]] = match *best_col_matches {
        Filled => return Some(word_rectangle.array),
        Unconstrained => unfiltered_col_candidates,
        OwnedMatches{matches:ref m} => m,
        BorrowedMatches{matches:m} => m,
      };
      let mut pb = if show_pb {
        Some(ProgressBar::new(matches.len() as u64))
      } else {
        None
      };
      for & word in matches {
        for p in & mut pb {p.inc();};
        let mut new_rectangle = word_rectangle.shallow_copy();
        new_rectangle.col_matches[best_col_ix] = Filled;
        {
          let target = new_rectangle.array.subview_mut(Axis(1),best_col_ix);
          Zip::from(word)
            .and(target)
            .apply(|& word_c, target_slot| *target_slot = Some(word_c));
          Zip::from(word)
            .and(& mut new_rectangle.row_matches)
            .apply(|&c, row| row.apply_constraint( &indices[&width], best_col_ix, c));
        }
        let child_result = step_word_rectangle(indices, words_by_length, new_rectangle, false);
        if child_result.is_some() {
          return child_result;
        }
      }
  }
  None
}

