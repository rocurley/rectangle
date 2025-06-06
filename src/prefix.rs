// This stores a tree of prefixes. Each node stores:
// * The prefix in question
// * The range of words that match the prefix
// * Pointers to all the prefixes 1-letter longer
use ascii::{AsciiChar, AsciiStr};

use crate::{BorrowedCrushedWords, CrushedWords};

#[derive(Debug)]
struct PrefixTree<'words> {
    prefix: &'words [AsciiChar],
    // Implicitly to be read in chunks of length n
    words: &'words [AsciiChar],
    children: Vec<Option<PrefixTree<'words>>>,
}
impl<'words> PrefixTree<'words> {
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
}

const EMPTY_ARRAY: [AsciiChar; 0] = [];

// TODO: this could be done with a single scan instead of l scans, which is probably more
// efficient.
fn construct_prefix_tree<'a>(words: BorrowedCrushedWords<'a>) -> PrefixTree<'a> {
    let mut root = PrefixTree {
        prefix: &EMPTY_ARRAY,
        words: &words.chars,
        children: Vec::with_capacity(26),
    };
    recurse(words.length, &mut root);
    root
}

fn recurse<'a>(l: usize, parent: &mut PrefixTree<'a>) {
    if parent.prefix.len() == l {
        return;
    }
    let alphabet =
        (AsciiChar::a.as_byte()..=AsciiChar::z.as_byte()).map(|b| AsciiChar::from(b).unwrap());
    let mut remaining_words = parent.words;
    for ch in alphabet {
        let split_idx = remaining_words
            .chunks_exact(l)
            .position(|word| word[parent.prefix.len()] > ch);
        let matching_words = match split_idx {
            Some(i) => {
                let split = i * l;
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
                prefix: &matching_words[..prefix_len],
                words: matching_words,
                children: Vec::with_capacity(26),
            };
            recurse(l, &mut child);
            parent.children.push(Some(child));
        }
    }
}
#[cfg(test)]
mod test {
    use ascii::AsciiStr;
    use proptest::collection::vec;
    use proptest::prelude::*;

    use super::{construct_prefix_tree, EMPTY_ARRAY};
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
            let tree = construct_prefix_tree(crushed_words.borrow());
            let ascii_prefix = AsciiStr::from_ascii(&prefix).unwrap();
            let actual_concat = tree.lookup(ascii_prefix.into()).map_or( EMPTY_ARRAY.as_slice(), |node| node.words,);
            let actual : Vec<_> = actual_concat.chunks_exact(3).map( |s| AsciiStr::as_str(s.into())).collect();
            let mut expected = words;
            expected.retain(|s| s.starts_with(&prefix));
            assert_eq!(expected, actual, "Failing tree: {:?}", &tree);
        }
    }
}
