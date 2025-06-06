// This stores a tree of prefixes. Each node stores:
// * The prefix in question
// * The range of words that match the prefix
// * Pointers to all the prefixes 1-letter longer
use ascii::AsciiChar;

use super::EMPTY_ARRAY;
use crate::BorrowedCrushedWords;

#[derive(Debug)]
pub struct PrefixTree<'words> {
    pub length: usize,
    pub prefix: &'words [AsciiChar],
    // Implicitly to be read in chunks of length length
    pub words: &'words [AsciiChar],
    children: Vec<Option<PrefixTree<'words>>>,
}
impl<'words> PrefixTree<'words> {
    // TODO: this could be done with a single scan instead of l scans, which is probably more
    // efficient.
    pub fn new<'a>(words: BorrowedCrushedWords<'words>) -> Self {
        let mut root = PrefixTree {
            length: words.length,
            prefix: &EMPTY_ARRAY,
            words: &words.chars,
            children: Vec::with_capacity(26),
        };
        recurse(&mut root);
        root
    }

    pub fn child(&self, ch: AsciiChar) -> Option<&Self> {
        let i = ch.as_byte() - AsciiChar::a.as_byte();
        self.children.get(i as usize)?.as_ref()
    }
    pub fn lookup(&self, prefix: &[AsciiChar]) -> Option<&Self> {
        match prefix.split_first() {
            None => Some(self),
            Some((ch, rest)) => {
                let child = self.child(*ch)?;
                child.lookup(rest)
            }
        }
    }
    pub fn get_word(&self, i: usize) -> &'words [AsciiChar] {
        &self.words[i * self.length..(i + 1) * self.length]
    }
    pub fn word_count(&self) -> usize {
        self.words.len() / self.length
    }
}

fn recurse<'a>(parent: &mut PrefixTree<'a>) {
    if parent.prefix.len() == parent.length {
        return;
    }
    let alphabet =
        (AsciiChar::a.as_byte()..=AsciiChar::z.as_byte()).map(|b| AsciiChar::from(b).unwrap());
    let mut remaining_words = parent.words;
    for ch in alphabet {
        let split_idx = remaining_words
            .chunks_exact(parent.length)
            .position(|word| word[parent.prefix.len()] > ch);
        let matching_words = match split_idx {
            Some(i) => {
                let split = i * parent.length;
                let matching_words = &remaining_words[..split];
                remaining_words = &remaining_words[split..];
                matching_words
            }
            None => {
                let matching_words = remaining_words;
                remaining_words = &EMPTY_ARRAY;
                matching_words
            }
        };
        if matching_words.is_empty() {
            parent.children.push(None);
        } else {
            let prefix_len = parent.prefix.len() + 1;
            let mut child = PrefixTree {
                length: parent.length,
                prefix: &matching_words[..prefix_len],
                words: matching_words,
                children: Vec::with_capacity(26),
            };
            recurse(&mut child);
            parent.children.push(Some(child));
        }
    }
}
#[cfg(test)]
mod test {
    use ascii::AsciiStr;
    use proptest::collection::vec;
    use proptest::prelude::*;

    use super::{PrefixTree, EMPTY_ARRAY};
    use crate::CrushedWords;

    proptest! {
        #[test]
        fn test_prefix_tree(
            mut words in vec("[a-c]{3}", 0..100),
            prefix in "[a-c]{0,3}",
            ) {
            words.sort();
            let crushed_words = CrushedWords {
                length: 3,
                chars: words
                    .iter()
                    .flat_map(|s| AsciiStr::from_ascii(s).unwrap().into_iter())
                    .copied()
                    .collect(),
            };
            let tree = PrefixTree::new(crushed_words.borrow());
            let ascii_prefix = AsciiStr::from_ascii(&prefix).unwrap();
            let actual_concat = tree.lookup(ascii_prefix.into()).map_or( EMPTY_ARRAY.as_slice(), |node| node.words,);
            let actual : Vec<_> = actual_concat.chunks_exact(3).map( |s| AsciiStr::as_str(s.into())).collect();
            let mut expected = words;
            expected.retain(|s| s.starts_with(&prefix));
            assert_eq!(expected, actual, "Failing tree: {:?}", &tree);
        }
    }
}
