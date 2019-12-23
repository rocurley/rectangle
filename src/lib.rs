#![allow(clippy::implicit_hasher)]
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

extern crate ascii;
use ascii::{AsciiChar, AsciiString};

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

extern crate itertools;
use itertools::join;

extern crate ndarray;
use ndarray::{Array2, Zip};

extern crate pbr;
use pbr::ProgressBar;

extern crate fnv;
use fnv::FnvHashMap;

extern crate typed_arena;
use typed_arena::Arena;

const EMPTY_ARRAY: [AsciiChar; 0] = [];
const EMPTY_NESTED_ARRAY: [&[AsciiChar]; 0] = [];

type Cache<'w> = FnvHashMap<u128, BorrowedSlotConstraint<'w>>;

#[derive(Debug, Clone, Copy)]
pub struct BorrowedSlotConstraint<'w> {
    matches: &'w [&'w [AsciiChar]],
    possible_chars: &'w [HashSet<AsciiChar>],
}

#[derive(Debug, Clone, Copy)]
pub enum WordsMatch<'w> {
    Unconstrained,
    Filled(&'w [AsciiChar]),
    BorrowedMatches {
        constraint: BorrowedSlotConstraint<'w>,
        prehash: u128,
    },
}

impl<'w> WordsMatch<'w> {
    pub fn fix_char(
        self,
        ix: usize,
        len: usize,
        c: AsciiChar,
        matches_slab: &'w Arena<Vec<&'w [AsciiChar]>>,
        possible_chars_slab: &'w Arena<Vec<HashSet<AsciiChar>>>,
        cache: &mut Cache<'w>,
    ) -> Self {
        match self {
            Filled(_) => panic!("cannot fix char in filled slot"),
            BorrowedMatches {
                constraint,
                prehash,
            } => {
                prehash |= (c as u128 - 'a' as u128 + 1) << (5 * (len - ix - 1));
                let new_constraint = cache.entry(prehash).or_insert_with(|| {
                    let matches = matches_slab
                        .alloc(
                            constraint
                                .matches
                                .iter()
                                .cloned()
                                .filter(|word| word[ix] == c)
                                .collect::<Vec<&'w [AsciiChar]>>(),
                        )
                        .as_slice();
                    let mut possible_chars_vec = vec![HashSet::new(); len];
                    for m in matches {
                        for (possible_char, charset) in m.iter().zip(possible_chars_vec.iter_mut())
                        {
                            charset.insert(*possible_char);
                        }
                    }
                    let possible_chars = possible_chars_slab.alloc(possible_chars_vec).as_slice();
                    BorrowedSlotConstraint {
                        matches,
                        possible_chars,
                    }
                });
                if new_constraint.matches.len() == 1 {
                    Filled(new_constraint.matches[0])
                } else {
                    BorrowedMatches {
                        constraint: *new_constraint,
                        prehash,
                    }
                }
            }
        }
    }
}
use WordsMatch::*;

pub struct CrushedWords {
    length: usize,
    chars: Vec<AsciiChar>,
}

impl<'a> FromIterator<&'a [AsciiChar]> for CrushedWords {
    fn from_iter<T>(x: T) -> Self
    where
        T: IntoIterator<Item = &'a [AsciiChar]>,
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
    pub fn push(&'a mut self, v: &[AsciiChar]) -> &'a [AsciiChar] {
        if self.chars.is_empty() {
            self.length = v.len();
        } else {
            assert_eq!(self.length, v.len());
        }
        let return_slice_start = self.chars.len();
        self.chars.extend_from_slice(&v);
        &self.chars[return_slice_start..]
    }
}

#[derive(Copy, Clone, Debug)]
pub struct BorrowedCrushedWords<'w> {
    length: usize,
    chars: &'w [AsciiChar],
}

impl<'w> IntoIterator for BorrowedCrushedWords<'w> {
    type Item = &'w [AsciiChar];
    type IntoIter = std::slice::ChunksExact<'w, AsciiChar>;
    fn into_iter(self) -> Self::IntoIter {
        self.chars.chunks_exact(self.length)
    }
}

impl<'w> BorrowedCrushedWords<'w> {
    fn len(self) -> usize {
        self.chars.len() / self.length
    }
    #[allow(dead_code)]
    fn empty() -> Self {
        BorrowedCrushedWords {
            length: 1,
            chars: &EMPTY_ARRAY,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Slot {
    Row { y: usize },
    Col { x: usize },
}
use Slot::*;

fn constraint_hash<'a, I>(iter: I) -> u128
where
    I: Iterator<Item = &'a Option<AsciiChar>>,
{
    let mut hash = 0;
    for option_c in iter {
        hash <<= 5;
        match option_c.as_ref() {
            None => {}
            Some(&letter) => hash += letter as u128 - 'a' as u128 + 1,
        };
    }
    hash
}

#[derive(Debug, Clone)]
pub struct WordRectangle<'w> {
    pub array: Array2<HashSet<AsciiChar>>,
    pub row_matches: Vec<WordsMatch<'w>>,
    pub col_matches: Vec<WordsMatch<'w>>,
}

impl<'w> WordRectangle<'w> {
    fn lookup_slot_matches(&self, slot: &Slot) -> &WordsMatch {
        match *slot {
            Row { y } => &self.row_matches[y],
            Col { x } => &self.col_matches[x],
        }
    }

    fn lookup_slot_matches_mut(&mut self, slot: &Slot) -> &mut WordsMatch<'w> {
        match *slot {
            Row { y } => &mut self.row_matches[y],
            Col { x } => &mut self.col_matches[x],
        }
    }

    fn width(&self) -> usize {
        self.array.shape()[1]
    }
    fn height(&self) -> usize {
        self.array.shape()[0]
    }

    fn apply_constraint<'a, 'b, 'c>(
        &'a self,
        (col_ix, row_ix): (usize, usize),
        c: AsciiChar,
        matches_slab: &'w Arena<Vec<&'w [AsciiChar]>>,
        possible_chars_slab: &'w Arena<Vec<HashSet<AsciiChar>>>,
        caches: &'c mut FnvHashMap<usize, Cache<'w>>,
    ) -> WordRectangle<'w> {
        let mut new_rectangle: WordRectangle<'w> = (*self).clone();
        let width = new_rectangle.width();
        let height = new_rectangle.height();
        let row_cache = caches.get_mut(&width).expect("Cache missing");
        let row_constraint = &mut self.row_matches[row_ix];
        *row_constraint = row_constraint.fix_char(
            col_ix,
            width,
            c,
            matches_slab,
            possible_chars_slab,
            row_cache,
        );
        let col_cache = caches.get_mut(&height).expect("Cache missing");
        let col_constraint = &mut self.col_matches[col_ix];
        *col_constraint = col_constraint.fix_char(
            row_ix,
            height,
            c,
            matches_slab,
            possible_chars_slab,
            col_cache,
        );
        match row_constraint {
            BorrowedMatches { constraint, .. } => {
                Zip::from(constraint.possible_chars)
                    .and(new_rectangle.array.gencolumns_mut())
                    .apply(|&possible_chars, col| {});
            }
        }
        new_rectangle
    }
}

pub fn show_word_rectangle(word_rectangle: &Array2<Option<AsciiChar>>) -> String {
    let rows = word_rectangle.outer_iter().map(|row| {
        row.iter()
            .map(|c| c.map_or('.', |c| c.as_char()))
            .collect::<String>()
    });
    join(rows, "\n")
}

pub fn step_word_rectangle<'w, 'a>(
    words_by_length: &'w HashMap<usize, BorrowedCrushedWords<'w>>,
    slab: &'w Arena<Vec<&'w [AsciiChar]>>,
    caches: &'a mut FnvHashMap<usize, FnvHashMap<u128, &'w [&'w [AsciiChar]]>>,
    word_rectangle: WordRectangle<'w>,
    show_pb: bool,
) -> (Option<Array2<Option<AsciiChar>>>, u64) {
    let width = word_rectangle.array.shape()[1];
    let height = word_rectangle.array.shape()[0];
    let unfiltered_row_candidates = words_by_length[&width];
    let unfiltered_col_candidates = words_by_length[&height];

    let (best_row_ix, best_row_matches) = word_rectangle
        .row_matches
        .iter()
        .enumerate()
        .min_by(|&(_, ref l_matches), &(_, ref r_matches)| l_matches.cmp(r_matches))
        .expect("Empty rows");
    let (best_col_ix, best_col_matches) = word_rectangle
        .col_matches
        .iter()
        .enumerate()
        .min_by(|&(_, ref l_matches), &(_, ref r_matches)| l_matches.cmp(r_matches))
        .expect("Empty cols");
    let target_slot = if (&best_row_matches, unfiltered_row_candidates.len())
        < (&best_col_matches, unfiltered_col_candidates.len())
    {
        Row { y: best_row_ix }
    } else {
        Col { x: best_col_ix }
    };
    let mut call_count = 1;
    match word_rectangle.lookup_slot_matches(&target_slot).0 {
        Filled => return (Some(word_rectangle.array.clone()), call_count),
        Unconstrained => {
            let matches = match target_slot {
                Row { .. } => unfiltered_row_candidates.into_iter(),
                Col { .. } => unfiltered_col_candidates.into_iter(),
            };
            let mut pb = if show_pb {
                Some(ProgressBar::new(matches.len() as u64))
            } else {
                None
            };
            for word in matches {
                for p in &mut pb {
                    p.inc();
                }
                {
                    let new_rectangle =
                        word_rectangle.apply_constraint(&target_slot, word, slab, caches);
                    let (child_result, child_call_count) =
                        step_word_rectangle(words_by_length, slab, caches, new_rectangle, false);
                    call_count += child_call_count;
                    if child_result.is_some() {
                        return (child_result, call_count);
                    }
                }
            }
        }
        BorrowedMatches { matches } => {
            let mut pb = if show_pb {
                Some(ProgressBar::new(matches.len() as u64))
            } else {
                None
            };
            for word in matches {
                for p in &mut pb {
                    p.inc();
                }
                {
                    let new_rectangle =
                        word_rectangle.apply_constraint(&target_slot, word, slab, caches);
                    let (child_result, child_call_count) =
                        step_word_rectangle(words_by_length, slab, caches, new_rectangle, false);
                    call_count += child_call_count;
                    if child_result.is_some() {
                        return (child_result, call_count);
                    }
                }
            }
        }
    };
    (None, call_count)
}

pub fn load_words(
    words_path: &str,
    min_len: usize,
    max_len: Option<usize>,
) -> HashMap<usize, CrushedWords> {
    let f = File::open(words_path).expect("Could not open file");
    let file = BufReader::new(&f);
    let words: Vec<AsciiString> = file
        .lines()
        .map(|line| line.expect("Not a line or something"))
        .filter(|word| {
            word.chars().all(|c| c.is_ascii_lowercase())
                && word.len() >= min_len
                && max_len.map_or(true, |max| word.len() < max)
        })
        .map(|word| AsciiString::from_ascii(word).expect("Somehow not ascii"))
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
    slab: &'w Arena<Vec<&'w [AsciiChar]>>,
    words_by_length: &'w HashMap<usize, CrushedWords>,
) -> FnvHashMap<usize, FnvHashMap<u128, &'w [&'w [AsciiChar]]>> {
    #[allow(clippy::type_complexity)]
    let mut indices: HashMap<usize, FnvHashMap<(usize, AsciiChar), Vec<&[AsciiChar]>>> =
        HashMap::new();
    for (l, words) in words_by_length {
        let index = indices.entry(*l).or_insert_with(FnvHashMap::default);
        for word in words.borrow() {
            for (pos, &c) in word.iter().enumerate() {
                index.entry((pos, c)).or_insert_with(Vec::new).push(word);
            }
        }
    }
    let mut caches: FnvHashMap<usize, FnvHashMap<u128, &[&[AsciiChar]]>> = FnvHashMap::default();
    for (l, index) in indices {
        let cache = caches.entry(l).or_insert_with(FnvHashMap::default);
        for ((pos, c), matches) in index.into_iter() {
            let mut key = vec![None; l];
            key[pos] = Some(c);
            cache.insert(constraint_hash(key.iter()), slab.alloc(matches).as_slice());
        }
    }
    caches
}
