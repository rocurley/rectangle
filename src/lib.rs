pub mod prefix;
pub mod word_rectangle;

use ascii::{AsciiChar, AsciiString};
use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter::FromIterator;

const EMPTY_ARRAY: [AsciiChar; 0] = [];

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

pub fn load_words(
    words_path: &str,
    min_len: usize,
    max_len: Option<usize>,
) -> HashMap<usize, CrushedWords> {
    let f = File::open(words_path).expect("Could not open file");
    let file = BufReader::new(&f);
    let mut words: Vec<AsciiString> = file
        .lines()
        .map(|line| line.expect("Not a line or something"))
        .filter(|word| {
            word.chars().all(|c| c.is_ascii_lowercase())
                && word.len() >= min_len
                && max_len.is_none_or(|max| word.len() < max)
        })
        .map(|word| AsciiString::from_ascii(word).expect("Somehow not ascii"))
        .collect();
    words.sort();
    let mut words_by_length = HashMap::new();
    for word in words.iter() {
        let l = word.len();
        let same_length = words_by_length.entry(l).or_insert_with(CrushedWords::empty);
        same_length.push(word.as_slice());
    }
    words_by_length
}
