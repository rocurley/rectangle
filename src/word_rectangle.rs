use super::prefix::PrefixTree;
use ascii::{AsciiChar, AsciiStr};
use itertools::join;
use std::collections::HashMap;
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

pub struct SolverStats {
    pub runtime: Duration,
    pub calls: u64,
}

enum BestCell {
    Complete,
    Failure,
    Cell { row: usize, mask: u32 },
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

    pub fn solve(mut self) -> (Option<Self>, SolverStats) {
        let mut calls = 0;
        let start = Instant::now();
        let solved = self.solve_inner(&mut calls);
        let out = if solved { Some(self) } else { None };
        let runtime = start.elapsed();
        let stats = SolverStats { calls, runtime };
        (out, stats)
    }
    fn solve_inner(&mut self, calls: &mut u64) -> bool {
        *calls += 1;
        let (row_ix, mask) = match self.best_cell() {
            BestCell::Complete => return true,
            BestCell::Failure => return false,
            BestCell::Cell { row, mask } => (row, mask),
        };
        let col_ix = self.row_matches[row_ix].prefix.len();
        for i in 0..26 {
            if (1 << i) & mask == 0 {
                continue;
            }
            let ch = AsciiChar::from(AsciiChar::a.as_byte() + i).unwrap();
            let row = &mut self.row_matches[row_ix];
            let col = &mut self.col_matches[col_ix];
            let row_backup = *row;
            let col_backup = *col;
            *row = row.child(ch).expect("invalid char for row");
            *col = col.child(ch).expect("invalid char for col");
            if self.solve_inner(calls) {
                return true;
            }
            self.row_matches[row_ix] = row_backup;
            self.col_matches[col_ix] = col_backup;
        }
        false
    }

    fn best_cell(&mut self) -> BestCell {
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
                return BestCell::Failure;
            }
            let count = mask.count_ones();
            if count < best_count {
                best_row = Some(i);
                best_count = count;
                best_mask = mask;
            }
        }
        match best_row {
            None => BestCell::Complete,
            Some(best_row) => BestCell::Cell {
                row: best_row,
                mask: best_mask,
            },
        }
    }
    pub fn show(&self) -> String {
        let row_strs = self.row_matches.iter().map(|row| {
            let s: &AsciiStr = row.prefix.into();
            s.as_str()
        });
        join(row_strs, "\n")
    }
}
