use super::prefix::PrefixTree;
use ascii::{AsciiChar, AsciiStr};
use itertools::join;
use std::collections::HashMap;
use std::iter::zip;
use std::time::{Duration, Instant};

#[derive(Debug)]
pub struct WordRectangle<'w> {
    pub rows_fixed: usize,
    pub cols_fixed: usize,
    pub row_matches: Vec<SlotContent<'w>>,
    pub col_matches: Vec<SlotContent<'w>>,
}

// TODO: annoying that this is 16 bytes.
#[derive(Debug, Clone)]
pub enum SlotContent<'w> {
    Possibilities(&'w PrefixTree<'w>),
    Word(&'w [AsciiChar]),
}

impl Clone for WordRectangle<'_> {
    fn clone(&self) -> Self {
        Self {
            rows_fixed: self.rows_fixed,
            cols_fixed: self.cols_fixed,
            row_matches: self.row_matches.clone(),
            col_matches: self.col_matches.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.rows_fixed = source.rows_fixed;
        self.cols_fixed = source.cols_fixed;
        self.row_matches.clone_from(&source.row_matches);
        self.col_matches.clone_from(&source.col_matches);
    }
}

#[derive(Debug, Copy, Clone)]
enum Slot {
    Row { y: usize },
    Col { x: usize },
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PickWordResult {
    Failure,
    Success,
}

pub struct SolverStats {
    pub runtime: Duration,
    pub calls: u64,
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

    fn lookup_slot_matches_mut<'a>(&'a mut self, slot: &Slot) -> &'a mut SlotContent<'w> {
        match *slot {
            Slot::Row { y } => &mut self.row_matches[y],
            Slot::Col { x } => &mut self.col_matches[x],
        }
    }

    fn pick_word(&mut self, slot: Slot, word_ix: usize) -> PickWordResult {
        match slot {
            Slot::Row { y } => {
                assert_eq!(y, self.rows_fixed);
                self.rows_fixed += 1;
            }
            Slot::Col { x } => {
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
            Slot::Row { y } => (&mut self.col_matches, y),
            Slot::Col { x } => (&mut self.row_matches, x),
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

    pub fn solve(self) -> (Option<Self>, SolverStats) {
        let mut scratch = Vec::new();
        let mut calls = 0;
        let start = Instant::now();
        let out = self.solve_inner(&mut scratch, &mut calls);
        let runtime = start.elapsed();
        let stats = SolverStats { calls, runtime };
        (out, stats)
    }
    fn solve_inner(self, scratch: &mut Vec<Self>, calls: &mut u64) -> Option<Self> {
        *calls += 1;
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
            let mut child = match scratch.pop() {
                Some(mut child) => {
                    child.clone_from(&self);
                    child
                }
                None => self.clone(),
            };
            if child.pick_word(slot, i) == PickWordResult::Failure {
                scratch.push(child);
                continue;
            }
            if let Some(solution) = child.solve_inner(scratch, calls) {
                return Some(solution);
            }
        }
        scratch.push(self);
        None
    }
    pub fn show(&self) -> String {
        let row_strs = self.row_matches.iter().map(|row| match row {
            SlotContent::Possibilities(_) => "?",
            SlotContent::Word(ascii_chars) => {
                let s: &AsciiStr = (*ascii_chars).into();
                s.as_str()
            }
        });
        join(row_strs, "\n")
    }
}
