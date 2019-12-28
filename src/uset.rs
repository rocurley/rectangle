use std::iter::FromIterator;
use std::mem::size_of;

type Block = u64;

#[derive(Debug, Clone)]
pub struct USet {
    blocks: Vec<Block>,
}

impl USet {
    pub fn new() -> Self {
        USet { blocks: Vec::new() }
    }
    pub fn populated_through(l: usize) -> Self {
        let full_blocks = l / size_of::<Block>();
        let trailing_bits = l % size_of::<Block>();
        if trailing_bits == 0 {
            return USet {
                blocks: vec![Block::max_value(); full_blocks],
            };
        }
        let mut blocks = vec![Block::max_value(); full_blocks + 1];
        blocks[full_blocks] = (1 << trailing_bits) - 1;
        USet { blocks }
    }
    pub fn insert(&mut self, x: usize) {
        let blocks_ix = x / size_of::<Block>();
        let shift = x % size_of::<Block>();
        if blocks_ix >= self.blocks.len() {
            self.blocks.resize(blocks_ix + 1, 0)
        }
        self.blocks[blocks_ix] |= 1 << shift;
    }
    pub fn intersect_with(&mut self, other: &Self) {
        if self.blocks.len() > other.blocks.len() {
            self.blocks.truncate(other.blocks.len());
        }
        for (l, r) in self.blocks.iter_mut().zip(other.blocks.iter()) {
            *l &= r;
        }
    }
    // NOTE: Does not appear to vectorize.
    pub fn has_intersection(&self, other: &Self) -> bool {
        self.blocks
            .iter()
            .zip(other.blocks.iter())
            .any(|(l, r)| (l & r) > 0)
    }
    pub fn difference_with(&mut self, other: &Self) {
        for (l, r) in self.blocks.iter_mut().zip(other.blocks.iter()) {
            *l &= !r;
        }
    }
    pub fn is_empty(&self) -> bool {
        self.blocks.iter().all(|&b| b == 0)
    }
}

impl Extend<usize> for USet {
    fn extend<T: IntoIterator<Item = usize>>(&mut self, iter: T) {
        for x in iter {
            self.insert(x);
        }
    }
}

impl FromIterator<usize> for USet {
    fn from_iter<T: IntoIterator<Item = usize>>(iter: T) -> Self {
        let mut out = Self::new();
        out.extend(iter);
        out
    }
}

#[cfg(test)]
mod uset_test;
