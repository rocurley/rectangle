use super::prefix::PrefixTree;
use ascii::{AsciiChar, AsciiStr};
use itertools::join;
use pbr::ProgressBar;
use std::collections::HashMap;
use std::iter::zip;
use std::time::{Duration, Instant};

#[derive(Debug)]
pub struct WordRectangle<'w> {
    pub row_matches: Vec<&'w PrefixTree<'w>>,
    pub col_matches: Vec<&'w PrefixTree<'w>>,
}

impl Clone for WordRectangle<'_> {
    fn clone(&self) -> Self {
        Self {
            row_matches: self.row_matches.clone(),
            col_matches: self.col_matches.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
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
        let row_matches = vec![row_tree; height];
        let col_tree = &indices[&width];
        let col_matches = vec![col_tree; width];
        WordRectangle {
            row_matches,
            col_matches,
        }
    }

    fn lookup_slot_matches_mut<'a>(&'a mut self, slot: &Slot) -> &'a mut &'w PrefixTree<'w> {
        match *slot {
            Slot::Row { y } => &mut self.row_matches[y],
            Slot::Col { x } => &mut self.col_matches[x],
        }
    }

    fn pick_char(&mut self, slot: Slot, char_ix: usize) -> PickWordResult {
        let tree = self.lookup_slot_matches_mut(&slot);
        let new_word = tree.get_word(word_ix);
        *slot_contents = SlotContent::Word(new_word);
        let (perp_slot, char_ix) = match slot {
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
        let out = self.solve_inner(&mut scratch, &mut calls, true);
        let runtime = start.elapsed();
        let stats = SolverStats { calls, runtime };
        (out, stats)
    }
    fn solve_inner(
        self,
        scratch: &mut Vec<Self>,
        calls: &mut u64,
        show_progress: bool,
    ) -> Option<Self> {
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
        let word_count = possibilities.word_count();
        let mut pb = if show_progress {
            let mut pb = ProgressBar::new(word_count as u64);
            pb.tick();
            Some(pb)
        } else {
            None
        };
        for i in 0..word_count {
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
            if let Some(solution) = child.solve_inner(scratch, calls, false) {
                return Some(solution);
            }
            if let Some(pb) = pb.as_mut() {
                pb.inc();
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
