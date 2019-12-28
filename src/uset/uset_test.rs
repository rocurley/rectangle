use super::USet;
#[test]
fn test_insert() {
    let mut set = USet::new();
    set.insert(3);
    assert_eq!(&set.blocks, &[0b1000]);
}

#[test]
fn test_has_intersection() {
    let a: USet = [1, 2, 3].iter().copied().collect();
    let b: USet = [0, 2, 3, 5].iter().copied().collect();
    let c: USet = [0, 5].iter().copied().collect();
    assert!(a.has_intersection(&b));
    assert!(!a.has_intersection(&c));
}

#[test]
fn test_intersect_with() {
    let mut a: USet = [1, 2, 3].iter().copied().collect();
    let b: USet = [0, 2, 3, 5].iter().copied().collect();
    a.intersect_with(&b);
    let one: USet = [1].iter().copied().collect();
    let two: USet = [2].iter().copied().collect();
    assert!(!a.has_intersection(&one));
    assert!(a.has_intersection(&two));
}
