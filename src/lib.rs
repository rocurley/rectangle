#![allow(clippy::implicit_hasher)]
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

use std::collections::HashMap;
use std::iter::FromIterator;
extern crate enumset;
use enumset::{EnumSet, EnumSetType};

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

const EMPTY_ARRAY: [Alpha; 0] = [];
const EMPTY_NESTED_ARRAY: [&[Alpha]; 0] = [];

#[derive(Debug, EnumSetType, Hash)]
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
}

type Cache<'w> = FnvHashMap<u128, BorrowedSlotConstraint<'w>>;

#[derive(Debug, Clone, Copy)]
pub struct BorrowedSlotConstraint<'w> {
    matches: &'w [&'w [Alpha]],
    possible_chars: &'w [EnumSet<Alpha>],
}

#[derive(Debug, Clone, Copy)]
pub enum WordsMatch<'w> {
    Unconstrained,
    Filled(&'w [Alpha]),
    BorrowedMatches {
        constraint: BorrowedSlotConstraint<'w>,
        prehash: u128,
    },
    NoMatches,
}

impl<'w> WordsMatch<'w> {
    pub fn fix_char(
        self,
        ix: usize,
        len: usize,
        c: Alpha,
        matches_slab: &'w Arena<Vec<&'w [Alpha]>>,
        possible_chars_slab: &'w Arena<Vec<EnumSet<Alpha>>>,
        cache: &mut Cache<'w>,
    ) -> Self {
        match self {
            Filled(word) => {
                assert_eq!(word[ix], c);
                return self;
            }
            NoMatches => panic!("cannot fix char if there are no matches"),
            BorrowedMatches {
                constraint,
                mut prehash,
            } => {
                prehash |= (c as u128 + 1) << (5 * (len - ix - 1));
                let new_constraint = cache.entry(prehash).or_insert_with(|| {
                    let matches = matches_slab
                        .alloc(
                            constraint
                                .matches
                                .iter()
                                .cloned()
                                .filter(|word| word[ix] == c)
                                .collect::<Vec<&'w [Alpha]>>(),
                        )
                        .as_slice();
                    let mut possible_chars_vec = vec![EnumSet::new(); len];
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
                match new_constraint.matches.len() {
                    0 => NoMatches,
                    1 => Filled(new_constraint.matches[0]),
                    _ => BorrowedMatches {
                        constraint: *new_constraint,
                        prehash,
                    },
                }
            }
            Unconstrained => {
                let prehash = (c as u128 + 1) << (5 * (len - ix - 1));
                // Cache has it if there's anything there, since we prepopulated it.
                cache
                    .get(&prehash)
                    .map_or(NoMatches, |&constraint| BorrowedMatches {
                        constraint,
                        prehash,
                    })
            }
        }
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
    I: Iterator<Item = &'a Option<Alpha>>,
{
    let mut hash = 0;
    for option_c in iter {
        hash <<= 5;
        match option_c.as_ref() {
            None => {}
            Some(&letter) => hash += letter as u128 + 1,
        };
    }
    hash
}

#[derive(Debug, Clone)]
pub struct WordRectangle<'w> {
    pub array: Array2<EnumSet<Alpha>>,
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
        (y, x): (usize, usize),
        c: Alpha,
        matches_slab: &'w Arena<Vec<&'w [Alpha]>>,
        possible_chars_slab: &'w Arena<Vec<EnumSet<Alpha>>>,
        caches: &'c mut FnvHashMap<usize, Cache<'w>>,
    ) -> Option<WordRectangle<'w>> {
        let mut new_rectangle: WordRectangle<'w> = (*self).clone();
        let width = new_rectangle.width();
        let height = new_rectangle.height();
        let mut to_fix = vec![(c, y, x)];
        while let Some((c, y, x)) = to_fix.pop() {
            let row_cache = caches.get_mut(&width).expect("Cache missing");
            let row_constraint = &mut new_rectangle.row_matches[y];
            *row_constraint =
                row_constraint.fix_char(x, width, c, matches_slab, possible_chars_slab, row_cache);
            let col_cache = caches.get_mut(&height).expect("Cache missing");
            let col_constraint = &mut new_rectangle.col_matches[x];
            *col_constraint =
                col_constraint.fix_char(y, height, c, matches_slab, possible_chars_slab, col_cache);
            match row_constraint {
                BorrowedMatches { constraint, .. } => {
                    Zip::indexed(constraint.possible_chars)
                        .and(new_rectangle.array.gencolumns_mut())
                        .apply(|ix, possible_chars, mut col| {
                            let prior_len = col[y].len();
                            col[y] = col[y].intersection(*possible_chars);
                            if col[y].len() == 1 && prior_len != 1 {
                                to_fix.push((
                                    col[y].iter().next().expect("Expected 1 possible char"),
                                    y,
                                    ix,
                                ));
                            }
                        });
                }
                Filled(word) => {
                    let legal = Zip::indexed(*word)
                        .and(new_rectangle.array.gencolumns_mut())
                        .all(|ix, &c, mut col| {
                            if !col[y].contains(c) {
                                return false;
                            }
                            if col[y].len() > 1 {
                                to_fix.push((c, y, ix));
                            }
                            col[y] = [c].iter().copied().collect();
                            true
                        });
                    if !legal {
                        return None;
                    }
                }
                Unconstrained => panic!("output of fix_char cannot be Unconstrained"),
                NoMatches => return None,
            }
            match col_constraint {
                BorrowedMatches { constraint, .. } => {
                    Zip::indexed(constraint.possible_chars)
                        .and(new_rectangle.array.genrows_mut())
                        .apply(|ix, possible_chars, mut row| {
                            let prior_len = row[x].len();
                            row[x] = row[x].intersection(*possible_chars);
                            if row[x].len() == 1 && prior_len != 1 {
                                to_fix.push((
                                    row[x].iter().next().expect("Expected 1 possible char"),
                                    ix,
                                    x,
                                ));
                            }
                        });
                }
                Filled(word) => {
                    let legal = Zip::indexed(*word)
                        .and(new_rectangle.array.genrows_mut())
                        .all(|ix, &c, mut row| {
                            if !row[x].contains(c) {
                                return false;
                            }
                            if row[x].len() > 1 {
                                to_fix.push((c, ix, x));
                            }
                            row[x] = [c].iter().copied().collect();
                            true
                        });
                    if !legal {
                        return None;
                    }
                }
                Unconstrained => panic!("output of fix_char cannot be Unconstrained"),
                NoMatches => return None,
            }
            let initial_possibilities: usize = self.array.iter().map(|s| s.len()).sum();
            let final_possibilities: usize = new_rectangle.array.iter().map(|s| s.len()).sum();
            if initial_possibilities <= final_possibilities {
                dbg!(self);
                dbg!(x, y);
                dbg!(new_rectangle);
                panic!("Not making progress")
            }
        }
        Some(new_rectangle)
    }
}

pub fn show_word_rectangle(word_rectangle: &Array2<Option<Alpha>>) -> String {
    let rows = word_rectangle.outer_iter().map(|row| {
        row.iter()
            .map(|c| c.map_or('.', |c| c.as_char()))
            .collect::<String>()
    });
    join(rows, "\n")
}

pub fn step_word_rectangle<'w, 'a>(
    words_by_length: &'w HashMap<usize, BorrowedCrushedWords<'w>>,
    matches_slab: &'w Arena<Vec<&'w [Alpha]>>,
    possible_chars_slab: &'w Arena<Vec<EnumSet<Alpha>>>,
    caches: &'a mut FnvHashMap<usize, Cache<'w>>,
    word_rectangle: WordRectangle<'w>,
    show_pb: bool,
) -> (Option<Array2<Option<Alpha>>>, u64) {
    let width = word_rectangle.array.shape()[1];
    let height = word_rectangle.array.shape()[0];

    //dbg!(&word_rectangle);
    let candidate = word_rectangle
        .array
        .indexed_iter()
        .map(|(ix, options)| (ix, options.len()))
        .filter(|(_, count)| *count != 1)
        .min_by_key(|(_, count)| *count);
    let target = match candidate {
        None => {
            //dbg!(&word_rectangle);
            return (
                Some(
                    word_rectangle
                        .array
                        .map(|possibilities| possibilities.into_iter().next()),
                ),
                1,
            );
        }
        Some((_, 0)) => return (None, 1),
        Some((idx, _)) => idx,
    };
    let options = &word_rectangle.array[target];
    let mut pb = if show_pb {
        Some(ProgressBar::new(options.len() as u64))
    } else {
        None
    };
    let mut call_count = 1;
    for c in options.iter() {
        if let Some(p) = pb.as_mut() {
            p.inc();
        }
        let new_rectangle = match word_rectangle.apply_constraint(
            target,
            c,
            matches_slab,
            possible_chars_slab,
            caches,
        ) {
            Some(rect) => rect,
            None => continue,
        };
        let (child_result, child_call_count) = step_word_rectangle(
            words_by_length,
            matches_slab,
            possible_chars_slab,
            caches,
            new_rectangle,
            false,
        );
        call_count += child_call_count;
        if child_result.is_some() {
            //dbg!(&word_rectangle);
            return (child_result, call_count);
        }
    }
    (None, call_count)
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
    matches_slab: &'w Arena<Vec<&'w [Alpha]>>,
    possible_chars_slab: &'w Arena<Vec<EnumSet<Alpha>>>,
    words_by_length: &'w HashMap<usize, CrushedWords>,
) -> FnvHashMap<usize, Cache<'w>> {
    #[allow(clippy::type_complexity)]
    let mut indices: HashMap<
        usize,
        FnvHashMap<(usize, Alpha), (Vec<&[Alpha]>, Vec<EnumSet<Alpha>>)>,
    > = HashMap::new();
    for (l, words) in words_by_length {
        let index = indices.entry(*l).or_insert_with(FnvHashMap::default);
        for word in words.borrow() {
            for (pos, &c) in word.iter().enumerate() {
                let (words, chars) = index
                    .entry((pos, c))
                    .or_insert_with(|| (Vec::new(), vec![EnumSet::new(); *l]));
                words.push(word);
                for (charset, c2) in chars.iter_mut().zip(word.iter()) {
                    charset.insert(*c2);
                }
            }
        }
    }
    let mut caches: FnvHashMap<usize, Cache> = FnvHashMap::default();
    for (l, index) in indices {
        let cache = caches.entry(l).or_insert_with(FnvHashMap::default);
        for ((pos, c), (matches, possible_chars)) in index.into_iter() {
            let mut key = vec![None; l];
            key[pos] = Some(c);
            let constraint = BorrowedSlotConstraint {
                matches: matches_slab.alloc(matches).as_slice(),
                possible_chars: possible_chars_slab.alloc(possible_chars),
            };
            cache.insert(constraint_hash(key.iter()), constraint);
        }
    }
    caches
}
