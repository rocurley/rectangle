#![feature(conservative_impl_trait)]
#![feature(ascii_ctype)]
#![feature(i128_type)]

use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;
use std::ascii::AsciiExt;

extern crate ascii;
use ascii::{AsciiString, AsciiChar};

use std::collections::HashMap;

use std::cmp::Ordering;

use std::time::Instant;

use std::rc::Rc;
use std::borrow::Borrow;

extern crate itertools;
use itertools::join;

extern crate ndarray;
use ndarray::{Array, Array2, Zip};

extern crate pbr;
use pbr::ProgressBar;

extern crate cpuprofiler;
use cpuprofiler::PROFILER;

extern crate fnv;
use fnv::FnvHashMap;

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
  let mut cache : FnvHashMap<(usize, u128) , Rc<[& [AsciiChar]]>> = FnvHashMap::default();
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
  for (l, index) in indices {
    println!("{}: {}", l, words_by_length[&l].len());
    for (i, matches) in index.into_iter().enumerate() {
      let pos = (i / 26) as u32;
      let c_int = (i % 26) as u128;
      let cache_hash = u128::pow(27,l as u32 - 1 - pos)*(c_int + 1);
      cache.insert((l,cache_hash), matches.into());
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
    match step_word_rectangle(& words_by_length, & mut cache, start, true) {
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

#[derive(Debug, Clone)]
enum WordsMatch <'w>{
  Unconstrained,
  Filled,
  BorrowedMatches { matches: Rc<[& 'w[AsciiChar]]>},
}
use WordsMatch::*;

impl<'w> Ord for WordsMatch<'w> {
  fn cmp(&self, other: & WordsMatch) -> Ordering {
    match(self, other) {
      (& Filled, & Filled) => Ordering::Equal,
      (& Filled, _) => Ordering::Greater,
      (_, & Filled) => Ordering::Less,
      (& Unconstrained, & Unconstrained) => Ordering::Equal,
      (& Unconstrained, _) => Ordering::Greater,
      (_, & Unconstrained) => Ordering::Less,
      (& BorrowedMatches{matches: ref l}, & BorrowedMatches{matches: ref r}) => l.len().cmp(& r.len())
    }
  }
}

impl<'w> PartialOrd for WordsMatch<'w> {
  fn partial_cmp(&self, other: &WordsMatch) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<'w> PartialEq for WordsMatch<'w> {
  fn eq(&self, other: & WordsMatch) -> bool {
    match self.cmp(other) {
      Ordering::Equal => true,
      _ => false,
    }
  }
}

impl<'w> Eq for WordsMatch<'w> {}

#[derive(Debug,Clone)]
enum Slot {
  Row {y : usize},
  Col {x : usize},
}
use Slot::*;

fn constraint_hash<'a, I>(iter : I) -> u128 where I : Iterator<Item=& 'a Option<AsciiChar>> {
  let mut hash = 0;
  for option_c in iter {
    hash *= 27;
    match option_c.as_ref() {
      None => {}
      Some(& c) => hash += c as u128 - 'a' as u128 + 1
    };
  }
  hash
}

#[derive(Debug,Clone)]
struct WordRectangle<'w> {
  array : Array2<Option<AsciiChar>>,
  row_matches : Vec<WordsMatch<'w>>,
  col_matches : Vec<WordsMatch<'w>>,
}

impl <'w> WordRectangle<'w> {

  fn lookup_slot_matches(& self, slot : & Slot) -> & WordsMatch {
    match slot {
      & Row{y} => & self.row_matches[y],
      & Col{x} => & self.col_matches[x],
    }
  }

  fn lookup_slot_matches_mut(& mut self, slot : & Slot) -> & mut WordsMatch<'w> {
    match slot {
      & Row{y} => & mut self.row_matches[y],
      & Col{x} => & mut self.col_matches[x],
    }
  }

  fn width(& self) -> usize {
    self.array.shape()[1]
  }
  fn height(& self) -> usize {
    self.array.shape()[0]
  }

  fn apply_constraint<'a, 'b, 'c> (
    & 'a self,
    slot : & Slot,
    word : & 'b [AsciiChar],
    cache : & 'c  mut FnvHashMap<(usize, u128), Rc<[& 'w [AsciiChar]]>>) -> WordRectangle<'w>{
    let mut new_rectangle : WordRectangle<'w> = (*self).clone();
    {
      let perp_len = match *slot {
        Row{..} => new_rectangle.height(),
        Col{..} =>  new_rectangle.width(),
      };
      let (perp_slots, perp_matches, pos) = match *slot {
        Row{y} => (new_rectangle.array.gencolumns_mut(), & mut new_rectangle.col_matches, y),
        Col{x} => (new_rectangle.array.genrows_mut()   , & mut new_rectangle.row_matches, x),
      };
      Zip::from(word)
        .and(perp_slots)
        .and(perp_matches)
        .apply(|& c, mut perp_slot, perp_match| {
          perp_slot[pos] = Some(c);
          if let & mut Filled = perp_match {
            return
          }
          let cache_entry = cache.entry((perp_len,constraint_hash(perp_slot.iter())));
          let matches : Rc<[& 'w [AsciiChar]]> = cache_entry.or_insert_with(|| {
            match perp_match {
              & mut Filled => panic!("We should have already returned"),
              & mut Unconstrained => panic!("Cache failed to contain {:?}", perp_slot),
              & mut BorrowedMatches{ref matches} => matches
                .iter()
                .cloned()
                .filter( |word| word[pos] == c)
                .collect::<Vec<& 'w [AsciiChar]>>()
                .into(),
            }
          }).clone();
          *perp_match = BorrowedMatches{matches}
        });
    }
    * new_rectangle.lookup_slot_matches_mut(slot) = Filled;
    new_rectangle
  }
}

fn show_word_rectangle(word_rectangle : & Array2<Option<AsciiChar>>) -> String {
  let rows = word_rectangle.outer_iter().map(|row|
    row.iter().map(|c| c.map_or('.',|c| c.as_char())).collect::<String>()
  );
  join(rows,"\n")
}

fn step_word_rectangle<'w, 'a>(
  words_by_length : & 'w HashMap<usize, Vec<& 'w[AsciiChar]>>,
  cache : & 'a mut FnvHashMap<(usize, u128), Rc<[& 'w [AsciiChar]]>>,
  word_rectangle : WordRectangle<'w>,
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
  let target_slot = if (& best_row_matches, unfiltered_row_candidates.len()) <
    (& best_col_matches, unfiltered_col_candidates.len()) {
    Row{y:best_row_ix}
  } else {
    Col{x:best_col_ix}
  };
  let matches : & [& [AsciiChar]] = match word_rectangle.lookup_slot_matches(& target_slot){
    & Filled => return Some(word_rectangle.array.clone()),
    & Unconstrained => match target_slot {
      Row{y:_} => unfiltered_row_candidates,
      Col{x:_} => unfiltered_col_candidates,
    }
    & BorrowedMatches{matches: ref m} => m.borrow(),
  };
  let mut pb = if show_pb {
    Some(ProgressBar::new(matches.len() as u64))
  } else {
    None
  };
  for & word in matches {
    for p in & mut pb {p.inc();};
    {
      let new_rectangle = word_rectangle.apply_constraint(& target_slot, word, cache);
      let child_result = step_word_rectangle(words_by_length, cache, new_rectangle, false);
      if child_result.is_some() {
        return child_result;
      }
    }
  }
  None
}

