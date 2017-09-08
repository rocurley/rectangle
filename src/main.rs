#![feature(ascii_ctype)]
#![feature(conservative_impl_trait)]

use std::env;
use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;
use std::ascii::AsciiExt;

use std::collections::HashMap;
use std::collections::HashSet;

use std::cmp::Ordering;

use std::time::Instant;

extern crate itertools;
use itertools::join;

extern crate ndarray;
use ndarray::{Array, Array2, Axis};

extern crate pbr;
use pbr::ProgressBar;

extern crate cpuprofiler;
use cpuprofiler::PROFILER;

extern crate fnv;
use fnv::FnvHashSet; //Reduced 14x13 from 20 to 16 secs.

fn main() {
  let words_path = env::args().nth(1).expect("No word file passed");
  let f = File::open(words_path).expect("Could not open file");
  let file = BufReader::new(&f);
  let mut counts : HashMap<usize, i32> = HashMap::new();
  let mut words_by_length : HashMap<usize, FnvHashSet<usize>> = HashMap::new();
  let mut indices : HashMap<usize, Vec<FnvHashSet<usize>>> = HashMap::new();
  let words : Vec<String> = file.lines()
    .map(|line| line.expect("Not a line or something"))
    .filter(|word| word.is_ascii_lowercase())
    .collect();
  let mut words_pb = ProgressBar::new(words.len() as u64);
  println!("Preprocessing words");
  for (word_ix, word) in words.iter().enumerate() {
    let l = word.len();

    let count = counts.entry(l).or_insert(0);
    *count += 1;

    let same_length = words_by_length.entry(l).or_insert(FnvHashSet::default());
    same_length.insert(word_ix);

    let index = indices.entry(l).or_insert(vec![FnvHashSet::default(); 26*l]);
    for (pos, c) in word.chars().enumerate() {
      index[ix(pos, c)].insert(word_ix);
    }

    words_pb.inc();
  };
  words_pb.finish_print("Preprocessing complete");
  /*
  for (k, v) in counts.iter() {
    println!("{}: {}", k, v);
  };
  */
  for (&k, index) in indices.iter() {
    println!("{}: {}", k, counts[&k]);
    for char_index in index.iter(){
      for &ix in char_index.iter() {
        let word = & words[ix];
        assert!(word.len() == k, "{} found in index for {}", word, k);
      }
    }
  }
  let mut dims = Vec::new();
  for x in counts.keys() {
    for y in counts.keys() {
      if x >= y {
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
          for new_rect in step_word_rectangle(& indices, & words_by_length, & words, rect) {
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
enum WordMatch {
  Unconstrained,
  Filled,
  Matches { matches: Vec<usize>}
}
use WordMatch::*;

impl Ord for WordMatch {
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

impl PartialOrd for WordMatch {
  fn partial_cmp(&self, other: &WordMatch) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl PartialEq for WordMatch {
  fn eq(&self, other: & WordMatch) -> bool {
    match self.cmp(other) {
      Ordering::Equal => true,
      _ => false,
    }
  }
}

impl Eq for WordMatch {}

fn _fold_hash_sets<'a, T, S>(mut sets : Vec<& 'a HashSet<T,S>>) -> Option<Vec<T>> where
  T : std::cmp::Eq  + std::hash::Hash + std::clone::Clone,
  S : std::hash::BuildHasher
  {
    sets.sort_by_key(|ref set| set.len());
    sets.split_first().map(|(& head, tail)| {
      let mut out : Vec<T> = head.iter().cloned().collect();//collect: 23%
      for set in tail{//retain: 12%
        out.retain(|x| set.contains(x));//contains: 43%
      }
      out
    })
  }

fn fold_hash_sets<'a, T, S>(mut sets : Vec<& 'a HashSet<T,S>>) -> Option<Vec<T>> where
  T : std::cmp::Eq  + std::hash::Hash + std::clone::Clone,
  S : std::hash::BuildHasher
  {
    sets.sort_by_key(|ref set| set.len());
    sets.split_first().map(|(& head, tail)| {
      let mut out = Vec::new();
      'outer: for x in head.iter(){//next: 47% (!?)
        for set in tail{
          if !set.contains(x){//contains: 19%
            continue 'outer;
          }
        }
        out.push(x.clone());//push: 16%
      }
      out
    })
  }

fn fit_word<'a, I>(to_fill : I, index : & Vec<FnvHashSet<usize>>) -> WordMatch where
  I : Iterator<Item = & 'a Option<char>>{
  //let mut matches : Option<FnvHashSet<usize>> = None;
  let mut full = true;
  let mut constraints = Vec::new();
  for (pos,&c_option) in to_fill.enumerate() {
    match c_option {
      Some(c) => {
        constraints.push(& index[ix(pos, c)]);
      }
      None => full = false
    };
  };
  let matches = fold_hash_sets(constraints);
  match (matches, full) {
    (_, true) => WordMatch::Filled,
    (None, false) => WordMatch::Unconstrained,
    (Some(matches), false) => WordMatch::Matches{matches : matches},
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

fn step_word_rectangle<'a>(
  indices : & HashMap<usize, Vec<FnvHashSet<usize>>>,
  words_by_length : & HashMap<usize, FnvHashSet<usize>>,
  words : & 'a Vec<String>,
  word_square : Array2<Option<char>>
  ) //-> impl Iterator<Item = Array2<Option<char>>> + 'a
  -> Box<Iterator<Item = Array2<Option<char>>> + 'a>
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
      let matches : Vec<usize> = match best_row_matches {
        Filled => panic!("Ran step_word_rectangle on full rectangle"),
        Unconstrained => unfiltered_row_candidates.iter().cloned().collect(),
        Matches{matches:m} => m,
      };
      Box::new(matches.into_iter().map(move |word_ix| {
        let mut new_square = word_square.clone();
        {
          let mut target = new_square.subview_mut(Axis(0),best_row_ix);
          let word = Array::from_vec(words[word_ix].chars().map(Some).collect());
          target.assign(& word);
        };
        new_square
      }))
  } else {
      //println!("Adding col");
      let matches : Vec<usize> = match best_col_matches {
        Filled => panic!("Ran step_word_rectangle on full rectangle"),
        Unconstrained => unfiltered_col_candidates.iter().cloned().collect(),
        Matches{matches:m} => m,
      };
      Box::new(matches.into_iter().map(move |word_ix| {
        let mut new_square = word_square.clone();
        {
          let mut target = new_square.subview_mut(Axis(1),best_col_ix);
          let word = Array::from_vec(words[word_ix].chars().map(Some).collect());
          target.assign(& word);
        };
        new_square
      }))
  }
}

