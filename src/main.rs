#![feature(conservative_impl_trait)]
#![feature(ascii_ctype)]

use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;
use std::ascii::AsciiExt;

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
  //let words_path = env::args().nth(1).expect("No word file passed");
  let f = File::open(words_path).expect("Could not open file");
  let file = BufReader::new(&f);
  let words : Vec<Vec<char>>;
  let mut words_by_length : HashMap<usize, Vec<& Vec<char>>> = HashMap::new();
  let mut indices : HashMap<usize, Vec<Vec<& Vec<char>>>> = HashMap::new();
  words = file.lines()
    .map(|line| line.expect("Not a line or something"))
    .filter(|word|
      word.is_ascii_lowercase() &&
      word.len() >= min_len &&
      max_len.map_or(true, |max| word.len() < max)
    ).map(|word| word.chars().collect())
    .collect();
  let mut words_pb = ProgressBar::new(words.len() as u64);
  println!("Preprocessing words");
  for word in words.iter() {
    let l = word.len();

    let same_length = words_by_length.entry(l).or_insert(Vec::new());
    same_length.push(word);

    let index = indices.entry(l).or_insert(vec![Vec::new(); 26*l]);
    for (pos, &c) in word.iter().enumerate() {
      index[ix(pos, c)].push(word);
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
    println!("{}x{}", w, h);
    let mut stack = vec![empty];
    let mut pb_tuple : Option<(ProgressBar<_>, u64)> = None;
    PROFILER.lock().unwrap().start(format!("profiling/{}x{}.profile", w,h)).unwrap();
    let start_time = Instant::now();
    loop {
      match stack.pop() {
        None => {
          let (ref mut pb,_) = pb_tuple.expect("Missing PB at end of loop");
          pb.finish_print("No rectangle found");
          break;
        },
        Some(rect) => {
          for new_rect in step_word_rectangle(& indices, & words_by_length, rect) {
            if complete_word_rectangle(& new_rect){
              println!("Found:\n{}", show_word_rectangle(& new_rect));
              return;
            };
            stack.push(new_rect);
          };
          match pb_tuple {
            None => {
              let count = stack.len() as u64;
              pb_tuple = Some((ProgressBar::new(count),count));
            },
            Some((ref mut pb,ref mut trigger)) => {
              if (stack.len() as u64) < *trigger {
                *trigger -= 1;
                assert!(*trigger == stack.len() as u64);
                pb.inc();
              }
            },
          };
        },
      };
    };
    let elapsed = start_time.elapsed();
    println!("{:?}", elapsed);
    PROFILER.lock().unwrap().stop().unwrap();
  };
}

fn ix(pos : usize, c : char) -> usize {
  assert!(c.is_ascii_lowercase());
  return pos*26+(c as usize - 'a' as usize)
}

#[derive(Debug)]
enum WordMatch <'a>{
  Unconstrained,
  Filled,
  Matches { matches: Vec<&'a Vec<char>>}
}
use WordMatch::*;

impl<'a> Ord for WordMatch<'a> {
  fn cmp(&self, other: & WordMatch) -> Ordering {
    match(self, other) {
      (& Filled, & Filled) => Ordering::Equal,
      (& Filled, _) => Ordering::Greater,
      (_, & Filled) => Ordering::Less,
      (& Unconstrained, & Unconstrained) => Ordering::Equal,
      (& Unconstrained, _) => Ordering::Greater,
      (_, & Unconstrained) => Ordering::Less,
      (& Matches{matches: ref l}, & Matches{matches: ref r}) => l.len().cmp(& r.len()),
    }
  }
}

impl<'a> PartialOrd for WordMatch<'a> {
  fn partial_cmp(&self, other: &WordMatch) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a> PartialEq for WordMatch<'a> {
  fn eq(&self, other: & WordMatch) -> bool {
    match self.cmp(other) {
      Ordering::Equal => true,
      _ => false,
    }
  }
}

impl<'a> Eq for WordMatch<'a> {}

fn fit_word<'a, 'b, 'w, I>(to_fill : I, index : &'b Vec<Vec<& 'w Vec<char>>>) -> WordMatch<'w> where
  I : Iterator<Item = & 'a Option<char>>{
  let mut full = true;
  let mut constraints : Vec<(usize, char)> = Vec::new();
  for (pos,&c_option) in to_fill.enumerate() {
    match c_option {
      Some(c) => {
        constraints.push((pos, c));
      }
      None => full = false
    };
  };
  constraints.sort_by_key(|&(pos, c)| index[ix(pos, c)].len());//Strictest first
  match (constraints.split_first(), full) {
    (_, true) => WordMatch::Filled,
    (None, false) => WordMatch::Unconstrained,
    (Some((&(seed_pos, seed_c), tail_constraints)), false) => {
      let mut matches : Vec<& 'w Vec<char>> = index[ix(seed_pos, seed_c)].clone();
      for &(pos, c) in tail_constraints{
        matches.retain(|word| word[pos] == c);
      }
      WordMatch::Matches{matches : matches}
    },
  }
}

fn show_word_rectangle(word_rectangle : & Array2<Option<char>>) -> String {
  let rows = word_rectangle.outer_iter().map(|row|
    row.iter().map(|c| c.unwrap_or('.')).collect::<String>()
  );
  join(rows,"\n")
}

fn complete_word_rectangle(word_rectangle : & Array2<Option<char>>) -> bool {
  word_rectangle.iter().all(|x| x.is_some())
}

fn step_word_rectangle<'a, 'w>(
  indices : & HashMap<usize, Vec<Vec<&'w Vec<char>>>>,
  words_by_length : & 'a HashMap<usize, Vec<&'w Vec<char>>>,
  word_square : Array2<Option<char>>
  ) -> Box<Iterator<Item = Array2<Option<char>>> + 'a>
  {
  let width = word_square.shape()[1];
  let height = word_square.shape()[0];
  //println!("{}x{}", width, height);
  let unfiltered_row_candidates = &words_by_length[&width];
  let unfiltered_col_candidates = &words_by_length[&height];

  let row_candidates = word_square.map_axis(Axis(1), |row|
    fit_word(row.iter(), &indices[&width])
  );
  let col_candidates = word_square.map_axis(Axis(0), |col|
    fit_word(col.iter(), &indices[&height])
  );
  /*
  println!("Expanding:\n{}", show_word_rectangle(& word_square));
  println!("Row Candidates");
  for r in row_candidates.iter(){
    println!("{:?}", r);
  };
  println!("Col Candidates");
  for r in col_candidates.iter(){
    println!("{:?}", r);
  };
  */
  let (best_row_ix, best_row_matches) =
    row_candidates.into_raw_vec().into_iter().enumerate()
      .min_by(|& (_, ref l_matches), & (_, ref r_matches)| l_matches.cmp(r_matches)).expect("Empty rows");
  let (best_col_ix, best_col_matches) =
    col_candidates.into_raw_vec().into_iter().enumerate()
      .min_by(|& (_, ref l_matches), & (_, ref r_matches)| l_matches.cmp(r_matches)).expect("Empty cols");
  if (& best_row_matches, unfiltered_row_candidates.len()) <
    (& best_col_matches, unfiltered_col_candidates.len()) {
      //println!("Adding row");
      let matches : Vec<& Vec<char>> = match best_row_matches {
        Filled => panic!("Ran step_word_rectangle on full rectangle"),
        Unconstrained => unfiltered_row_candidates.iter().cloned().collect(),
        Matches{matches:m} => m,
      };
      Box::new(matches.into_iter().map(move |word| {
        let mut new_square = word_square.clone();
        {
          let target = new_square.subview_mut(Axis(0),best_row_ix);
          Zip::from(word)
            .and(target)
            .apply(|& word_c, target_slot| *target_slot = Some(word_c));
        };
        new_square
      }))
  } else {
      //println!("Adding col");
      let matches : Vec<& Vec<char>> = match best_col_matches {
        Filled => panic!("Ran step_word_rectangle on full rectangle"),
        Unconstrained => unfiltered_col_candidates.iter().cloned().collect(),
        Matches{matches:m} => m,
      };
      Box::new(matches.into_iter().map(move |word| {
        let mut new_square = word_square.clone();
        {
          let target = new_square.subview_mut(Axis(1),best_col_ix);
          Zip::from(word)
            .and(target)
            .apply(|& word_c, target_slot| *target_slot = Some(word_c));
        };
        new_square
      }))
  }
}

