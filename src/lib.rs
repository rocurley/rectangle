pub mod prefix;

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

extern crate ascii;
use ascii::AsciiStr;
use ascii::{AsciiChar, AsciiString};
use prefix::PrefixTree;

use std::cmp::Ordering;
use std::collections::HashMap;
use std::iter::{zip, FromIterator};

extern crate itertools;
use itertools::join;

extern crate ndarray;
use ndarray::Array2;

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
    BorrowedMatches { matches: &'w [&'w [AsciiChar]] },
}
use crate::WordsMatch::*;

impl Ord for WordsMatch<'_> {
    fn cmp(&self, other: &WordsMatch) -> Ordering {
        match (self, other) {
            (&Filled, &Filled) => Ordering::Equal,
            (&Filled, _) => Ordering::Greater,
            (_, &Filled) => Ordering::Less,
            (&Unconstrained, &Unconstrained) => Ordering::Equal,
            (&Unconstrained, _) => Ordering::Greater,
            (_, &Unconstrained) => Ordering::Less,
            (&BorrowedMatches { matches: l }, &BorrowedMatches { matches: r }) => {
                l.len().cmp(&r.len())
            }
        }
    }
}

impl PartialOrd for WordsMatch<'_> {
    fn partial_cmp(&self, other: &WordsMatch) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for WordsMatch<'_> {
    fn eq(&self, other: &WordsMatch) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for WordsMatch<'_> {}

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
    pub fn borrow(&'a self) -> BorrowedCrushedWords<'a> {
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
        self.chars.extend_from_slice(v);
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

impl BorrowedCrushedWords<'_> {
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

#[derive(Debug, Copy, Clone)]
pub enum Slot {
    Row { y: usize },
    Col { x: usize },
}
use crate::Slot::*;

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

// TODO: annoying that this is 16 bytes.
#[derive(Debug, Clone)]
enum SlotContent<'w> {
    Possibilities(&'w PrefixTree<'w>),
    Word(&'w [AsciiChar]),
}

#[derive(Debug, Clone)]
pub struct WordRectangle<'w> {
    pub rows_fixed: usize,
    pub cols_fixed: usize,
    pub row_matches: Vec<SlotContent<'w>>,
    pub col_matches: Vec<SlotContent<'w>>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PickWordResult {
    Failure,
    Success,
}

impl<'w> WordRectangle<'w> {
    pub fn new(
        width: usize,
        height: usize,
        indices: &'w HashMap<usize, PrefixTree<'w>>,
    ) -> Self {
        let row_tree = &indices[&width];
        let row_matches = vec![SlotContent::Possibilities(row_tree); height];
        let col_tree = &indices[&width];
        let col_matches = vec![SlotContent::Possibilities(col_tree); width];
        WordRectangle {
            rows_fixed: 0,
            cols_fixed: 0,
            row_matches,
            col_matches,
        }
    }
    fn lookup_slot_matches(&self, slot: &Slot) -> &SlotContent {
        match *slot {
            Row { y } => &self.row_matches[y],
            Col { x } => &self.col_matches[x],
        }
    }

    fn lookup_slot_matches_mut<'a>(&'a mut self, slot: &Slot) -> &'a mut SlotContent<'w> {
        match *slot {
            Row { y } => &mut self.row_matches[y],
            Col { x } => &mut self.col_matches[x],
        }
    }

    fn width(&self) -> usize {
        self.col_matches.len()
    }
    fn height(&self) -> usize {
        self.row_matches.len()
    }

    fn pick_word(&mut self, slot: Slot, word_ix: usize) -> PickWordResult {
        match slot {
            Row { y } => {
                assert_eq!(y, self.rows_fixed);
                self.rows_fixed += 1;
            }
            Col { x } => {
                assert_eq!(x, self.cols_fixed);
                self.cols_fixed += 1;
            }
        };
        let slot_contents = self.lookup_slot_matches_mut(&slot);
        let SlotContent::Possibilities(tree) = *slot_contents else {
            panic!("Tried to pick word when word was already fixed");
        };
        let new_word = tree.get_word(word_ix);
        *slot_contents = SlotContent::Word(new_word);
        let (perp_slots, char_ix) = match slot {
            Row { y } => (&mut self.col_matches, y),
            Col { x } => (&mut self.row_matches, x),
        };
        for (contents, ch) in zip(perp_slots, new_word) {
            let SlotContent::Possibilities(tree) = *contents else {
                continue;
            };
            assert_eq!(char_ix, tree.prefix.len());
            let Some(new) = tree.child(*ch) else {
                return PickWordResult::Failure;
            };
            *contents = SlotContent::Possibilities(new);
        }
        PickWordResult::Success
    }

    pub fn solve(self) -> Option<Self> {
        let (slot, possibilities) = match (
            self.row_matches.get(self.rows_fixed),
            self.col_matches.get(self.cols_fixed),
        ) {
            (None, None) => return Some(self),
            (None, Some(SlotContent::Possibilities(p))) => {
                (Slot::Col { x: self.cols_fixed }, p)
            }
            (Some(SlotContent::Possibilities(p)), None) => {
                (Slot::Row { y: self.rows_fixed }, p)
            }
            (Some(SlotContent::Possibilities(row)), Some(SlotContent::Possibilities(col))) => {
                if row.word_count() < col.word_count() {
                    (Slot::Row { y: self.rows_fixed }, row)
                } else {
                    (Slot::Col { x: self.cols_fixed }, col)
                }
            }
            _ => panic!("Current slot has a word already set"),
        };
        for i in 0..possibilities.word_count() {
            let mut child = self.clone();
            if child.pick_word(slot, i) == PickWordResult::Failure {
                continue;
            }
            if let Some(solution) = child.solve() {
                return Some(solution);
            }
        }
        None
    }
    pub fn show(&self) -> String {
        let row_strs = self.row_matches.iter().map(|row| match row {
            SlotContent::Possibilities(prefix_tree) => "?",
            SlotContent::Word(ascii_chars) => {
                let s: &AsciiStr = (*ascii_chars).into();
                s.as_str()
            }
        });
        join(row_strs, "\n")
    }
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
        let index = indices.entry(*l).or_default();
        for word in words.borrow() {
            for (pos, &ch) in word.iter().enumerate() {
                index.entry((pos, ch)).or_default().push(word);
            }
        }
    }
    let mut caches: FnvHashMap<usize, FnvHashMap<u128, &[&[AsciiChar]]>> =
        FnvHashMap::default();
    for (l, index) in indices {
        let cache = caches.entry(l).or_default();
        for ((pos, ch), matches) in index.into_iter() {
            let mut key = vec![None; l];
            key[pos] = Some(ch);
            cache.insert(constraint_hash(key.iter()), slab.alloc(matches).as_slice());
        }
    }
    caches
}
