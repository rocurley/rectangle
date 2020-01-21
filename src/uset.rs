use std::cmp::Ordering;
use std::iter::FromIterator;
use std::iter::Peekable;
use std::mem::size_of;

type Block = u64;

const MIN_INVERSE_DENSITY: usize = 8;
const MAX_INVERSE_DENSITY: usize = 16;

#[derive(Debug, Clone)]
pub enum USet {
    Dense { blocks: Vec<Block>, count: usize },
    Sparse(Vec<usize>),
}

fn insert_blocks(blocks: &mut Vec<Block>, count: &mut usize, x: usize) {
    let blocks_ix = x / size_of::<Block>();
    let shift = x % size_of::<Block>();
    if blocks_ix >= blocks.len() {
        blocks.resize(blocks_ix + 1, 0)
    }
    let mask = 1 << shift;
    if mask & blocks[blocks_ix] == 0 {
        *count += 1;
    }
    blocks[blocks_ix] |= 1 << shift;
}

fn remove_blocks(blocks: &mut Vec<Block>, count: &mut usize, x: usize) {
    let blocks_ix = x / size_of::<Block>();
    let shift = x % size_of::<Block>();
    if blocks_ix >= blocks.len() {
        return;
    }
    let mask = 1 << shift;
    if mask & blocks[blocks_ix] > 0 {
        *count -= 1;
    }
    blocks[blocks_ix] &= !(1 << shift);
}

fn iter_blocks<'a>(blocks: &'a Vec<Block>) -> impl Iterator<Item = usize> + 'a {
    blocks.iter().enumerate().flat_map(|(blocks_ix, block)| {
        (0..size_of::<Block>()).filter_map(move |shift| {
            let mask = 1 << shift;
            if mask & block > 0 {
                Some(blocks_ix * size_of::<Block>() + shift)
            } else {
                None
            }
        })
    })
}

impl USet {
    pub fn new() -> Self {
        USet::Dense {
            blocks: Vec::new(),
            count: 0,
        }
    }
    pub fn populated_through(l: usize) -> Self {
        let full_blocks = l / size_of::<Block>();
        let trailing_bits = l % size_of::<Block>();
        if trailing_bits == 0 {
            return USet::Dense {
                blocks: vec![Block::max_value(); full_blocks],
                count: l,
            };
        }
        let mut blocks = vec![Block::max_value(); full_blocks + 1];
        blocks[full_blocks] = (1 << trailing_bits) - 1;
        USet::Dense {
            blocks: blocks,
            count: l,
        }
    }
    pub fn insert(&mut self, x: usize) {
        match self {
            USet::Dense { blocks, count } => {
                insert_blocks(blocks, count, x);
            }
            USet::Sparse(items) => match items.binary_search(&x) {
                Err(ix) => items.insert(ix, x),
                Ok(_) => {}
            },
        }
    }
    pub fn intersect_with(&mut self, other: &Self) {
        match (&mut *self, other) {
            (
                USet::Dense {
                    blocks: self_blocks,
                    count,
                },
                USet::Dense {
                    blocks: other_blocks,
                    ..
                },
            ) => {
                if self_blocks.len() > other_blocks.len() {
                    self_blocks.truncate(other_blocks.len());
                }
                *count = 0;
                for (l, r) in self_blocks.iter_mut().zip(other_blocks.iter()) {
                    *l &= r;
                    *count += l.count_ones() as usize;
                }
                self.recheck();
            }
            (
                USet::Sparse(self_items),
                USet::Dense {
                    blocks: other_blocks,
                    ..
                },
            ) => self_items.retain(|&x| lookup_in_blocks(other_blocks, x)),
            (
                USet::Dense {
                    blocks: self_blocks,
                    ..
                },
                USet::Sparse(other_items),
            ) => {
                let tmp = USet::Sparse(
                    other_items
                        .iter()
                        .copied()
                        .filter(|&x| lookup_in_blocks(self_blocks, x))
                        .collect(),
                );
                *self = tmp;
            }
            (USet::Sparse(self_items), USet::Sparse(other_items)) => {
                let mut other_iter = other_items.iter().peekable();
                self_items.retain(|x| {
                    while let Some(y) = other_iter.peek() {
                        match x.cmp(y) {
                            Ordering::Equal => {
                                other_iter.next();
                                return true;
                            }
                            Ordering::Less => {
                                return false;
                            }
                            Ordering::Greater => {
                                other_iter.next();
                            }
                        }
                    }
                    false
                })
            }
        }
    }
    pub fn union_with(&mut self, other: &Self) {
        match (&mut *self, other) {
            (
                USet::Dense {
                    blocks: self_blocks,
                    count,
                },
                USet::Dense {
                    blocks: other_blocks,
                    ..
                },
            ) => {
                *count = 0;
                for (l, r) in self_blocks.iter_mut().zip(other_blocks.iter()) {
                    *l |= r;
                    *count += l.count_ones() as usize;
                }
                if self_blocks.len() < other_blocks.len() {
                    self_blocks.extend_from_slice(&other_blocks[self_blocks.len()..]);
                }
            }
            (USet::Dense { blocks, count }, USet::Sparse(items)) => {
                for &x in items {
                    insert_blocks(blocks, count, x);
                }
            }
            (USet::Sparse(items), USet::Dense { blocks, count }) => {
                let mut out_blocks = blocks.clone();
                let mut out_count = *count;
                for &x in items.iter() {
                    insert_blocks(&mut out_blocks, &mut out_count, x);
                }
                *self = USet::Dense {
                    blocks: out_blocks,
                    count: out_count,
                };
            }
            (USet::Sparse(self_items), USet::Sparse(other_items)) => {
                *self = SortedUnion {
                    i1: self_items.iter().copied().peekable(),
                    i2: other_items.iter().copied().peekable(),
                }
                .collect();
                self.recheck();
            }
        }
    }
    pub fn has_intersection(&self, other: &Self) -> bool {
        match (self, other) {
            // NOTE: Does not appear to vectorize.
            (
                USet::Dense {
                    blocks: self_blocks,
                    ..
                },
                USet::Dense {
                    blocks: other_blocks,
                    ..
                },
            ) => self_blocks
                .iter()
                .zip(other_blocks.iter())
                .any(|(l, r)| (l & r) > 0),
            (USet::Dense { blocks, .. }, USet::Sparse(items))
            | (USet::Sparse(items), USet::Dense { blocks, .. }) => {
                items.iter().any(|&x| lookup_in_blocks(blocks, x))
            }
            (USet::Sparse(self_items), USet::Sparse(other_items)) => SortedIntersection {
                i1: self_items.iter().peekable(),
                i2: other_items.iter().peekable(),
            }
            .next()
            .is_some(),
        }
    }
    pub fn difference_with(&mut self, other: &Self) {
        match (&mut *self, other) {
            (
                USet::Dense {
                    blocks: self_blocks,
                    count,
                },
                USet::Dense {
                    blocks: other_blocks,
                    ..
                },
            ) => {
                *count = 0;
                for (l, r) in self_blocks.iter_mut().zip(other_blocks.iter()) {
                    *l &= !r;
                    *count += l.count_ones() as usize;
                }
                self.recheck();
            }
            (
                USet::Sparse(self_items),
                USet::Dense {
                    blocks: other_blocks,
                    ..
                },
            ) => self_items.retain(|&x| !lookup_in_blocks(other_blocks, x)),
            (USet::Dense { blocks, count }, USet::Sparse(other_items)) => {
                for item in other_items {
                    remove_blocks(blocks, count, *item);
                }
                self.recheck();
            }
            (USet::Sparse(self_items), USet::Sparse(other_items)) => {
                let mut other_iter = other_items.iter().peekable();
                self_items.retain(|x| {
                    while let Some(y) = other_iter.peek() {
                        match x.cmp(y) {
                            Ordering::Equal => {
                                other_iter.next();
                                return false;
                            }
                            Ordering::Less => {
                                return true;
                            }
                            Ordering::Greater => {
                                other_iter.next();
                            }
                        }
                    }
                    true
                })
            }
        }
    }
    pub fn is_empty(&self) -> bool {
        match self {
            USet::Dense { count, .. } => *count == 0,
            USet::Sparse(items) => items.is_empty(),
        }
    }
    fn recheck(&mut self) {
        match self {
            USet::Dense { blocks, count } => {
                if *count * MAX_INVERSE_DENSITY < blocks.len() {
                    let out_items = iter_blocks(blocks).collect();
                    *self = USet::Sparse(out_items);
                }
            }
            USet::Sparse(items) => {
                if let Some(last) = items.last() {
                    if items.len() * MIN_INVERSE_DENSITY > *last {
                        let mut blocks = Vec::new();
                        let mut count = 0;
                        for item in items {
                            insert_blocks(&mut blocks, &mut count, *item);
                        }
                        *self = USet::Dense { blocks, count };
                    }
                }
            }
        }
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

fn lookup_in_blocks(blocks: &[Block], x: usize) -> bool {
    let blocks_ix = x / size_of::<Block>();
    let shift = x % size_of::<Block>();
    if let Some(block) = blocks.get(blocks_ix) {
        block & (1 << shift) > 0
    } else {
        false
    }
}

struct SortedIntersection<T, I1, I2>
where
    I1: Iterator<Item = T>,
    I2: Iterator<Item = T>,
    T: Ord,
{
    i1: Peekable<I1>,
    i2: Peekable<I2>,
}

impl<T, I1, I2> Iterator for SortedIntersection<T, I1, I2>
where
    I1: Iterator<Item = T>,
    I2: Iterator<Item = T>,
    T: Ord,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        while let (Some(n1), Some(n2)) = (self.i1.peek(), self.i2.peek()) {
            match (*n1).cmp(n2) {
                Ordering::Equal => {
                    self.i1.next();
                    return self.i2.next();
                }
                Ordering::Less => {
                    self.i1.next();
                }
                Ordering::Greater => {
                    self.i2.next();
                }
            }
        }
        None
    }
}

struct SortedUnion<T, I1, I2>
where
    I1: Iterator<Item = T>,
    I2: Iterator<Item = T>,
    T: Ord,
{
    i1: Peekable<I1>,
    i2: Peekable<I2>,
}

impl<T, I1, I2> Iterator for SortedUnion<T, I1, I2>
where
    I1: Iterator<Item = T>,
    I2: Iterator<Item = T>,
    T: Ord,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match (self.i1.peek(), self.i2.peek()) {
            (Some(n1), Some(n2)) => match (*n1).cmp(n2) {
                Ordering::Equal => {
                    self.i1.next();
                    self.i2.next()
                }
                Ordering::Less => self.i1.next(),
                Ordering::Greater => self.i2.next(),
            },
            (Some(_), None) => self.i1.next(),
            (None, Some(_)) => self.i2.next(),
            (None, None) => None,
        }
    }
}

#[cfg(test)]
mod uset_test;
