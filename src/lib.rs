use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

extern crate ascii;
use ascii::{AsciiChar, AsciiString};

use std::cmp::Ordering;
use std::collections::HashMap;
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

#[derive(Debug, Clone)]
pub enum WordsMatch<'w> {
    Unconstrained,
    Filled,
    BorrowedMatches { matches: BorrowedCrushedWords<'w> },
}
use WordsMatch::*;

impl<'w> Ord for WordsMatch<'w> {
    fn cmp(&self, other: &WordsMatch) -> Ordering {
        match (self, other) {
            (&Filled, &Filled) => Ordering::Equal,
            (&Filled, _) => Ordering::Greater,
            (_, &Filled) => Ordering::Less,
            (&Unconstrained, &Unconstrained) => Ordering::Equal,
            (&Unconstrained, _) => Ordering::Greater,
            (_, &Unconstrained) => Ordering::Less,
            (&BorrowedMatches { matches: ref l }, &BorrowedMatches { matches: ref r }) => {
                l.raw_len().cmp(&r.raw_len())
            }
        }
    }
}

impl<'w> PartialOrd for WordsMatch<'w> {
    fn partial_cmp(&self, other: &WordsMatch) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'w> PartialEq for WordsMatch<'w> {
    fn eq(&self, other: &WordsMatch) -> bool {
        match self.cmp(other) {
            Ordering::Equal => true,
            _ => false,
        }
    }
}

impl<'w> Eq for WordsMatch<'w> {}

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
    pub fn empty() -> Self {
        CrushedWords {
            length: 1,
            chars: Vec::new(),
        }
    }
    pub fn push(&mut self, v: &[AsciiChar]) {
        if self.chars.len() == 0 {
            self.length = v.len();
        } else {
            assert_eq!(self.length, v.len());
        }
        self.chars.extend_from_slice(&v);
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
    fn raw_len(self) -> usize {
        self.chars.len()
    }
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
        hash = hash << 5;
        match option_c.as_ref() {
            None => {}
            Some(&c) => hash += c as u128 - 'a' as u128 + 1,
        };
    }
    hash
}

#[derive(Debug, Clone)]
pub struct WordRectangle<'w> {
    pub array: Array2<Option<AsciiChar>>,
    pub row_matches: Vec<WordsMatch<'w>>,
    pub col_matches: Vec<WordsMatch<'w>>,
}

impl<'w> WordRectangle<'w> {
    fn lookup_slot_matches(&self, slot: &Slot) -> &WordsMatch {
        match slot {
            &Row { y } => &self.row_matches[y],
            &Col { x } => &self.col_matches[x],
        }
    }

    fn lookup_slot_matches_mut(&mut self, slot: &Slot) -> &mut WordsMatch<'w> {
        match slot {
            &Row { y } => &mut self.row_matches[y],
            &Col { x } => &mut self.col_matches[x],
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
        slot: &Slot,
        word: &'b [AsciiChar],
        slab: &'w Arena<CrushedWords>,
        caches: &'c mut FnvHashMap<usize, FnvHashMap<u128, BorrowedCrushedWords<'w>>>,
    ) -> WordRectangle<'w> {
        let mut new_rectangle: WordRectangle<'w> = (*self).clone();
        {
            let perp_len = match *slot {
                Row { .. } => new_rectangle.height(),
                Col { .. } => new_rectangle.width(),
            };
            let cache = caches.get_mut(&perp_len).expect("Cache missing");
            let (perp_slots, perp_matches, pos) = match *slot {
                Row { y } => (
                    new_rectangle.array.gencolumns_mut(),
                    &mut new_rectangle.col_matches,
                    y,
                ),
                Col { x } => (
                    new_rectangle.array.genrows_mut(),
                    &mut new_rectangle.row_matches,
                    x,
                ),
            };
            Zip::from(word).and(perp_slots).and(perp_matches).apply(
                |&c, mut perp_slot, perp_match| {
                    perp_slot[pos] = Some(c);
                    if let &mut Filled = perp_match {
                        return;
                    }
                    let cache_entry = cache.entry(constraint_hash(perp_slot.iter()));
                    let matches: BorrowedCrushedWords<'w> = cache_entry
                        .or_insert_with(|| match perp_match {
                            &mut Filled => panic!("We should have already returned"),
                            // All single-character constraints are pre-populated into the cache,
                            // so a cache miss here means there's nothing to find.
                            &mut Unconstrained => BorrowedCrushedWords::empty(),
                            &mut BorrowedMatches { ref matches } => slab
                                .alloc(
                                    matches
                                        .into_iter()
                                        .filter(|word| word[pos] == c)
                                        .collect::<CrushedWords>(),
                                )
                                .borrow(),
                        })
                        .clone();
                    *perp_match = BorrowedMatches { matches }
                },
            );
        }
        *new_rectangle.lookup_slot_matches_mut(slot) = Filled;
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
    slab: &'w Arena<CrushedWords>,
    caches: &'a mut FnvHashMap<usize, FnvHashMap<u128, BorrowedCrushedWords<'w>>>,
    word_rectangle: WordRectangle<'w>,
    show_pb: bool,
) -> Option<Array2<Option<AsciiChar>>> {
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
    let matches: BorrowedCrushedWords = match word_rectangle.lookup_slot_matches(&target_slot) {
        &Filled => return Some(word_rectangle.array.clone()),
        &Unconstrained => match target_slot {
            Row { y: _ } => unfiltered_row_candidates,
            Col { x: _ } => unfiltered_col_candidates,
        },
        &BorrowedMatches { matches: m } => m,
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
            let new_rectangle = word_rectangle.apply_constraint(&target_slot, word, slab, caches);
            let child_result =
                step_word_rectangle(words_by_length, slab, caches, new_rectangle, false);
            if child_result.is_some() {
                return child_result;
            }
        }
    }
    None
}

pub fn preprocess_words<'w>(
    words: &'w mut Vec<AsciiString>,
    slab: &'w Arena<CrushedWords>,
    words_path: &str,
    min_len: usize,
    max_len: Option<usize>,
) -> (
    HashMap<usize, CrushedWords>,
    FnvHashMap<usize, FnvHashMap<u128, BorrowedCrushedWords<'w>>>,
) {
    let f = File::open(words_path).expect("Could not open file");
    let file = BufReader::new(&f);
    let words: Vec<AsciiString>;
    let mut words_by_length: HashMap<usize, CrushedWords> = HashMap::new();
    let mut indices: HashMap<usize, FnvHashMap<(usize, AsciiChar), CrushedWords>> = HashMap::new();
    let mut caches: FnvHashMap<usize, FnvHashMap<u128, BorrowedCrushedWords>> =
        FnvHashMap::default();
    words = file
        .lines()
        .map(|line| line.expect("Not a line or something"))
        .filter(|word| {
            word.chars().all(|c| c.is_ascii_lowercase())
                && word.len() >= min_len
                && max_len.map_or(true, |max| word.len() < max)
        })
        .map(|word| AsciiString::from_ascii(word).expect("Somehow not ascii"))
        .collect();
    let mut words_pb = ProgressBar::new(words.len() as u64);
    println!("Preprocessing words");
    for word in words.iter() {
        let l = word.len();

        let same_length = words_by_length.entry(l).or_insert(CrushedWords::empty());
        same_length.push(word.as_slice());

        let index = indices.entry(l).or_insert(FnvHashMap::default());
        for (pos, &c) in word.chars().enumerate() {
            index
                .entry((pos, c))
                .or_insert(CrushedWords::empty())
                .push(word.as_slice());
        }

        words_pb.inc();
    }
    words_pb.finish_print("Preprocessing complete");
    for (l, index) in indices {
        let cache = caches.entry(l).or_insert(FnvHashMap::default());
        for ((pos, c), matches) in index.into_iter() {
            let mut key = vec![None; l];
            key[pos] = Some(c);
            cache.insert(constraint_hash(key.iter()), slab.alloc(matches).borrow());
        }
    }
    (words_by_length, caches)
}
