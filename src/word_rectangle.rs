use super::prefix::PrefixTree;
use ascii::{AsciiChar, AsciiStr};
use itertools::join;
use std::collections::HashMap;
use std::time::{Duration, Instant};
use std::usize;

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

    fn pick_char(&mut self, row_ix: usize, ch: AsciiChar) {
        let row = &mut self.row_matches[row_ix];
        let col = &mut self.col_matches[row.prefix.len()];
        *row = row.child(ch).expect("invalid char for row");
        *col = col.child(ch).expect("invalid char for col");
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
        let mut best_row = None;
        let mut best_count = u32::MAX;
        let mut best_mask = 0;
        let mut prior_prefix_len = usize::MAX;
        for (i, &row) in self.row_matches.iter().enumerate() {
            let col_ready = row.prefix.len() < prior_prefix_len;
            prior_prefix_len = row.prefix.len();
            // col tree hasn't reached this row yet
            if !col_ready {
                continue;
            }
            // Row is complete
            if row.prefix.len() == self.col_matches.len() {
                continue;
            }
            let col = self.col_matches[row.prefix.len()];
            let mask = row.valid_children & col.valid_children;
            // No possible values for this cell: short-circuit.
            if mask == 0 {
                scratch.push(self);
                return None;
            }
            let count = mask.count_ones();
            if count < best_count {
                best_row = Some(i);
                best_count = count;
                best_mask = mask;
            }
        }
        let Some(row_ix) = best_row else {
            // All rows complete: we found it!
            return Some(self);
        };
        for i in 0..26 {
            if (1 << i) & best_mask == 0 {
                continue;
            }
            let mut child = match scratch.pop() {
                Some(mut child) => {
                    child.clone_from(&self);
                    child
                }
                None => self.clone(),
            };
            let ch = AsciiChar::from(AsciiChar::a.as_byte() + i).unwrap();
            child.pick_char(row_ix, ch);
            if let Some(solution) = child.solve_inner(scratch, calls) {
                return Some(solution);
            }
        }
        scratch.push(self);
        None
    }
    pub fn show(&self) -> String {
        let row_strs = self.row_matches.iter().map(|row| {
            let s: &AsciiStr = row.prefix.into();
            s.as_str()
        });
        join(row_strs, "\n")
    }
}
