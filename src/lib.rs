#![allow(clippy::implicit_hasher)]
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
extern crate bit_set;
use bit_set::BitSet;
extern crate bit_vec;
use bit_vec::BitVec;
use std::collections::HashMap;
use std::iter::FromIterator;
extern crate enumset;
use enumset::{EnumSet, EnumSetType};
extern crate enum_map;
use enum_map::{Enum, EnumMap};
extern crate itertools;
use itertools::join;
use std::hash::BuildHasher;

extern crate pbr;
use pbr::ProgressBar;

extern crate fnv;
use fnv::FnvHashMap;

const EMPTY_ARRAY: [Alpha; 0] = [];

#[derive(Debug, EnumSetType, Hash, Enum)]
pub enum Alpha {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
}
impl Alpha {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            'a' => Some(Alpha::A),
            'b' => Some(Alpha::B),
            'c' => Some(Alpha::C),
            'd' => Some(Alpha::D),
            'e' => Some(Alpha::E),
            'f' => Some(Alpha::F),
            'g' => Some(Alpha::G),
            'h' => Some(Alpha::H),
            'i' => Some(Alpha::I),
            'j' => Some(Alpha::J),
            'k' => Some(Alpha::K),
            'l' => Some(Alpha::L),
            'm' => Some(Alpha::M),
            'n' => Some(Alpha::N),
            'o' => Some(Alpha::O),
            'p' => Some(Alpha::P),
            'q' => Some(Alpha::Q),
            'r' => Some(Alpha::R),
            's' => Some(Alpha::S),
            't' => Some(Alpha::T),
            'u' => Some(Alpha::U),
            'v' => Some(Alpha::V),
            'w' => Some(Alpha::W),
            'x' => Some(Alpha::X),
            'y' => Some(Alpha::Y),
            'z' => Some(Alpha::Z),
            _ => None,
        }
    }
    pub fn as_char(self) -> char {
        match self {
            Alpha::A => 'a',
            Alpha::B => 'b',
            Alpha::C => 'c',
            Alpha::D => 'd',
            Alpha::E => 'e',
            Alpha::F => 'f',
            Alpha::G => 'g',
            Alpha::H => 'h',
            Alpha::I => 'i',
            Alpha::J => 'j',
            Alpha::K => 'k',
            Alpha::L => 'l',
            Alpha::M => 'm',
            Alpha::N => 'n',
            Alpha::O => 'o',
            Alpha::P => 'p',
            Alpha::Q => 'q',
            Alpha::R => 'r',
            Alpha::S => 's',
            Alpha::T => 't',
            Alpha::U => 'u',
            Alpha::V => 'v',
            Alpha::W => 'w',
            Alpha::X => 'x',
            Alpha::Y => 'y',
            Alpha::Z => 'z',
        }
    }
    pub fn from_str(word: &str) -> Option<Vec<Self>> {
        word.chars()
            .map(Alpha::from_char)
            .collect::<Option<Vec<Alpha>>>()
    }
    pub fn to_string(word: &[Self]) -> String {
        word.iter().copied().map(Alpha::as_char).collect()
    }
}

#[derive(Debug)]
pub struct Counters {
    pub step_calls: usize,
    pub reduce_calls: usize,
    pub reduce_cells: usize,
    pub ban_calls: usize,
}
impl Counters {
    pub fn new() -> Self {
        Counters {
            step_calls: 0,
            reduce_calls: 0,
            reduce_cells: 0,
            ban_calls: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PartialConstraint {
    words: BitSet,
    possible_chars: Vec<EnumSet<Alpha>>,
}

#[derive(Debug, Clone)]
pub struct Cache {
    positions: Vec<EnumMap<Alpha, PartialConstraint>>,
    unconstrained: PartialConstraint,
}

#[derive(Debug, Clone)]
pub enum WordsMatch<'w> {
    Unconstrained,
    Filled(&'w [Alpha]),
    // TODO: add a prehash here if needed.
    Matches(PartialConstraint),
    NoMatches,
}

impl<'w> WordsMatch<'w> {
    pub fn possible_chars(&self, cache: &Cache, ix: usize) -> EnumSet<Alpha> {
        match self {
            Unconstrained => cache.unconstrained.possible_chars[ix],
            Filled(word) => EnumSet::only(word[ix]),
            Matches(constraint) => constraint.possible_chars[ix],
            NoMatches => EnumSet::empty(),
        }
    }
    pub fn fix_char(self, ix: usize, c: Alpha, cache: &Cache) -> Self {
        match self {
            Filled(word) => {
                if word[ix] == c {
                    Filled(word)
                } else {
                    NoMatches
                }
            }
            NoMatches => NoMatches,
            Matches(mut constraint) => {
                let cache_entry = &cache.positions[ix][c];
                constraint.words.intersect_with(&cache_entry.words);
                Matches(constraint)
            }
            Unconstrained => Matches(cache.positions[ix][c].clone()),
        }
    }
    pub fn ban_char(self, ix: usize, c: Alpha, cache: &Cache) -> Self {
        match self {
            Filled(word) => {
                if word[ix] == c {
                    NoMatches
                } else {
                    Filled(word)
                }
            }
            NoMatches => NoMatches,
            Matches(mut constraint) => {
                let cache_entry = &cache.positions[ix][c];
                constraint.words.difference_with(&cache_entry.words);
                if constraint.words.is_empty() {
                    NoMatches
                } else {
                    Matches(constraint)
                }
            }
            Unconstrained => Matches(cache.unconstrained.clone()).ban_char(ix, c, cache),
        }
    }
    pub fn fix_char_mut(&mut self, ix: usize, c: Alpha, cache: &Cache) {
        let mut tmp = WordsMatch::NoMatches;
        std::mem::swap(&mut tmp, self);
        *self = tmp.fix_char(ix, c, cache);
    }
    pub fn ban_char_mut(&mut self, ix: usize, c: Alpha, cache: &Cache) {
        let mut tmp = WordsMatch::NoMatches;
        std::mem::swap(&mut tmp, self);
        *self = tmp.ban_char(ix, c, cache);
    }
}

fn filter_chars(matches: &mut WordsMatch, cache: &Cache) {
    let constraint = if let Matches(constraint) = matches {
        constraint
    } else {
        return;
    };
    let PartialConstraint {
        possible_chars,
        words,
    } = constraint;
    for (position_chars, cache_position) in possible_chars.iter_mut().zip(&cache.positions) {
        *position_chars = position_chars
            .iter()
            .filter(|c| {
                // Require a word with c at this position to be in words.
                words
                    .intersection(&cache_position[*c].words)
                    .next()
                    .is_some()
            })
            .collect()
    }
}

use WordsMatch::*;

pub struct CrushedWords {
    length: usize,
    chars: Vec<Alpha>,
}

impl<'a> FromIterator<&'a [Alpha]> for CrushedWords {
    fn from_iter<T>(x: T) -> Self
    where
        T: IntoIterator<Item = &'a [Alpha]>,
    {
        let mut words = CrushedWords::empty();
        for word in x {
            words.push(word);
        }
        words
    }
}

impl<'a> CrushedWords {
    pub fn borrow(&'a self) -> BorrowedCrushedWords {
        BorrowedCrushedWords {
            length: self.length,
            chars: &self.chars,
        }
    }
    pub fn len(&self) -> usize {
        self.chars.len() / self.length
    }
    pub fn is_empty(&self) -> bool {
        self.chars.is_empty()
    }
    pub fn empty() -> Self {
        CrushedWords {
            length: 1,
            chars: Vec::new(),
        }
    }
    pub fn push(&'a mut self, v: &[Alpha]) -> &'a [Alpha] {
        if self.chars.is_empty() {
            self.length = v.len();
        } else {
            assert_eq!(self.length, v.len());
        }
        let return_slice_start = self.chars.len();
        self.chars.extend_from_slice(&v);
        &self.chars[return_slice_start..]
    }
    pub fn word_len(&self) -> usize {
        self.length
    }
}

#[derive(Copy, Clone, Debug)]
pub struct BorrowedCrushedWords<'w> {
    length: usize,
    chars: &'w [Alpha],
}

impl<'w> IntoIterator for BorrowedCrushedWords<'w> {
    type Item = &'w [Alpha];
    type IntoIter = std::slice::ChunksExact<'w, Alpha>;
    fn into_iter(self) -> Self::IntoIter {
        self.chars.chunks_exact(self.length)
    }
}

impl<'w> BorrowedCrushedWords<'w> {
    pub fn len(self) -> usize {
        self.chars.len() / self.length
    }
    pub fn empty() -> Self {
        BorrowedCrushedWords {
            length: 1,
            chars: &EMPTY_ARRAY,
        }
    }
}

#[derive(Debug, Clone)]
pub struct WordRectangle<'w> {
    pub row_matches: Vec<WordsMatch<'w>>,
    pub col_matches: Vec<WordsMatch<'w>>,
    pub row_cache: &'w Cache,
    pub col_cache: &'w Cache,
}

impl<'w> WordRectangle<'w> {
    pub fn width(&self) -> usize {
        self.col_matches.len()
    }
    pub fn height(&self) -> usize {
        self.row_matches.len()
    }
    fn reduce(mut self, counters: &mut Counters) -> Option<Self> {
        counters.reduce_calls += 1;
        'restart: loop {
            for (y, row) in self.row_matches.iter_mut().enumerate() {
                let row_chars = match row {
                    Unconstrained => self.row_cache.unconstrained.possible_chars.clone(),
                    Filled(word) => word.iter().copied().map(EnumSet::only).collect(),
                    Matches(constraint) => constraint.possible_chars.clone(),
                    NoMatches => return None,
                };
                for (x, (col, &row_char)) in self
                    .col_matches
                    .iter_mut()
                    .zip(row_chars.iter())
                    .enumerate()
                {
                    let col_char = match col {
                        Unconstrained => self.col_cache.unconstrained.possible_chars[y],
                        Filled(word) => EnumSet::only(word[y]),
                        Matches(constraint) => constraint.possible_chars[y],
                        NoMatches => return None,
                    };
                    let diff = row_char.symmetrical_difference(col_char);
                    if !diff.is_empty() {
                        counters.reduce_cells += 1;
                        let mut did_something = false;
                        if (diff & row_char).len() > 1 {
                            for banned_char in diff & row_char {
                                counters.ban_calls += 1;
                                row.ban_char_mut(x, banned_char, self.row_cache);
                            }
                            filter_chars(row, self.row_cache);
                            did_something = true;
                        }
                        if (diff & col_char).len() > 1 {
                            for banned_char in diff & col_char {
                                counters.ban_calls += 1;
                                col.ban_char_mut(y, banned_char, self.col_cache);
                            }
                            filter_chars(col, self.col_cache);
                            did_something = true;
                        }
                        if did_something {
                            continue 'restart;
                        }
                    }
                }
            }
            return Some(self);
        }
    }
    fn find_fork_point(&self) -> Option<(usize, usize)> {
        self.row_matches
            .iter()
            .enumerate()
            .filter_map(|(y, row)| match row {
                WordsMatch::Matches(constraint) => Some((y, constraint)),
                WordsMatch::Filled(_) => None,
                WordsMatch::NoMatches => panic!("Called find_fork_point on dead rectangle"),
                WordsMatch::Unconstrained => Some((y, &self.row_cache.unconstrained)),
            })
            .flat_map(|(y, constraint)| {
                constraint
                    .possible_chars
                    .iter()
                    .enumerate()
                    .map(move |(x, possible_chars)| (y, x, possible_chars))
            })
            .filter(|(_, _, possible_chars)| possible_chars.len() > 1)
            .min_by_key(|(_, _, possible_chars)| possible_chars.len())
            .map(|(y, x, _)| (y, x))
    }
    pub fn show(&self) -> String {
        let mut unconstrained_string = String::new();
        for _ in 0..self.col_matches.len() {
            unconstrained_string.push('?');
        }
        let mut no_matches_string = String::new();
        for _ in 0..self.col_matches.len() {
            no_matches_string.push('.');
        }
        let rows = self.row_matches.iter().map(|row| match row {
            Unconstrained => unconstrained_string.clone(),
            Filled(word) => Alpha::to_string(word),
            Matches(constraint) => constraint
                .possible_chars
                .iter()
                .map(|cs| match cs.len() {
                    0 => '.',
                    1 => cs.iter().next().expect("Should have one char").as_char(),
                    _ => '?',
                })
                .collect(),
            NoMatches => no_matches_string.clone(),
        });
        join(rows, "\n")
    }
    pub fn new<S: BuildHasher>(w: usize, h: usize, caches: &'w HashMap<usize, Cache, S>) -> Self {
        let mut row_matches = Vec::new();
        for _ in 0..h {
            row_matches.push(Unconstrained)
        }
        let mut col_matches = Vec::new();
        for _ in 0..w {
            col_matches.push(Unconstrained)
        }
        WordRectangle {
            row_matches,
            col_matches,
            row_cache: &caches[&w],
            col_cache: &caches[&h],
        }
    }
}

pub fn step_word_rectangle<'w>(
    word_rectangle: WordRectangle<'w>,
    show_pb: bool,
    recursion_depth: u16,
    counters: &mut Counters,
) -> Option<WordRectangle<'w>> {
    if recursion_depth > (word_rectangle.width() * word_rectangle.height()) as u16 + 1 {
        panic!("Exceeded max recursion depth")
    }
    let reduced_option = word_rectangle.reduce(counters);
    let reduced = match reduced_option {
        None => return None,
        Some(r) => r,
    };
    let (y, x) = match reduced.find_fork_point() {
        None => return Some(reduced),
        Some(fork) => fork,
    };
    let options = reduced.row_matches[y].possible_chars(reduced.row_cache, x);
    //dbg!(&reduced.row_matches, &reduced.col_matches);
    //dbg!(y, x, options);
    let mut pb = if show_pb {
        Some(ProgressBar::new(options.len() as u64))
    } else {
        None
    };
    counters.step_calls += 1;
    for c in options.into_iter() {
        let mut fixed = reduced.clone();
        fixed.row_matches[y].fix_char_mut(x, c, fixed.row_cache);
        filter_chars(&mut fixed.row_matches[y], fixed.row_cache);
        fixed.col_matches[x].fix_char_mut(y, c, fixed.col_cache);
        filter_chars(&mut fixed.col_matches[x], fixed.col_cache);
        let res = step_word_rectangle(fixed, false, recursion_depth + 1, counters);
        if let Some(success) = res {
            return Some(success);
        }
        if let Some(p) = pb.as_mut() {
            p.inc();
        }
    }
    None
}

pub fn load_words(
    words_path: &str,
    min_len: usize,
    max_len: Option<usize>,
) -> HashMap<usize, CrushedWords> {
    let f = File::open(words_path).expect("Could not open file");
    let file = BufReader::new(&f);
    let words: Vec<Vec<Alpha>> = file
        .lines()
        .map(|line| line.expect("Not a line or something"))
        .filter(|word| {
            word.chars().all(|c| c.is_ascii_lowercase())
                && word.len() >= min_len
                && max_len.map_or(true, |max| word.len() < max)
        })
        .map(|word| Alpha::from_str(&word).expect("Somehow not ascii"))
        .collect();
    let mut words_by_length = HashMap::new();
    for word in words.iter() {
        let l = word.len();
        let same_length = words_by_length.entry(l).or_insert_with(CrushedWords::empty);
        same_length.push(word.as_slice());
    }
    words_by_length
}

pub fn prepopulate_cache<'w>(
    words_by_length: &'w HashMap<usize, CrushedWords>,
) -> FnvHashMap<usize, Cache> {
    words_by_length
        .iter()
        .map(|(&l, words)| (l, create_cache(words)))
        .collect()
}

pub fn create_cache(words: &CrushedWords) -> Cache {
    let l = words.word_len();
    let empty_map = (|_| PartialConstraint {
        words: BitSet::new(),
        possible_chars: vec![EnumSet::new(); l],
    })
    .into();
    let mut positions: Vec<EnumMap<Alpha, PartialConstraint>> = vec![empty_map; l];
    let mut unconstrained = PartialConstraint {
        words: BitSet::from_bit_vec(BitVec::from_elem(words.len(), true)),
        possible_chars: vec![EnumSet::new(); l],
    };
    for (word_ix, word) in words.borrow().into_iter().enumerate() {
        for ((&c, unconstrained_possible_chars), position) in word
            .iter()
            .zip(unconstrained.possible_chars.iter_mut())
            .zip(positions.iter_mut())
        {
            *unconstrained_possible_chars |= c;
            position[c].words.insert(word_ix);
            for (&c2, possible_chars) in word.iter().zip(position[c].possible_chars.iter_mut()) {
                *possible_chars |= c2
            }
        }
    }
    Cache {
        positions,
        unconstrained,
    }
}

#[cfg(test)]
mod reduce_test;
#[cfg(test)]
mod step_test;
