extern crate adapton;

use std::rc::Rc;
use adapton::engine::*;
use adapton::engine::manage::*;
use adapton::catalog::collections::trie::*;

#[test]
fn test_is_empty() {
    init_dcg();
    let meta = Meta { min_depth: 1 };
    let empty = TrieIntro::<usize>::empty(meta.clone());
    let singleton = Trie::singleton(meta.clone(), name_unit(), 7 as usize);
    assert!(Trie::<usize>::is_empty(&TrieIntro::empty(meta.clone())));
    assert!(Trie::is_empty(&empty));

    assert!(!Trie::is_empty(&Trie::singleton(meta.clone(), name_unit(), 0 as usize)));
    assert!(!Trie::is_empty(&singleton));
}

#[test]
fn test_equal() {
    init_dcg();
    let meta = Meta { min_depth: 1 };
    let empty: Trie<usize> = TrieIntro::empty(meta.clone());
    let singleton_7 = Trie::singleton(meta.clone(), name_of_usize(7), 7);
    let singleton_7_ = Trie::singleton(meta.clone(), name_of_usize(7), 7);
    let singleton_8 = Trie::singleton(meta.clone(), name_of_usize(8), 8);
    assert_eq!(empty, empty);
    assert_eq!(singleton_7, singleton_7);
    assert_eq!(singleton_7, singleton_7_);
    assert_eq!(singleton_8, singleton_8);

    assert_ne!(empty, singleton_7);
    assert_ne!(empty, singleton_8);
    assert_ne!(singleton_7, singleton_8);
}

// Set membership is consistent after additions.
#[test]
fn test_set() {
    init_dcg();
    let e: Set<usize> = SetIntro::empty();
    assert!(!Set::mem(&e, &7));
    assert!(!Set::mem(&e, &1));
    let s = SetIntro::add(e, 7);
    let s = SetIntro::add(s, 1);
    let s = SetIntro::add(s, 8);
    assert!(Set::mem(&s, &1));
    assert!(Set::mem(&s, &7));
    assert!(Set::mem(&s, &8));
    assert!(!Set::mem(&s, &0));
}

// Order in which elements are added to sets doesn't matter.
#[test]
fn test_set_equal() {
    init_dcg();
    let e: Set<usize> = SetIntro::empty();
    let s = SetIntro::add(e, 7);
    let s = SetIntro::add(s, 1);
    let s = SetIntro::add(s, 8);

    let e: Set<usize> = SetIntro::empty();
    let t = SetIntro::add(e, 8);
    let t = SetIntro::add(t, 7);
    let t = SetIntro::add(t, 1);
    assert_eq!(s, t);
}


fn push_input(i: usize, t: Trie<usize>) -> Trie<usize> {
    let t = Trie::art(cell(name_of_usize(i), t));
    let t = Trie::name(name_of_usize(i), t);
    Trie::extend(name_unit(), t, i)
}

#[test]
fn test_set_fold() {
    init_dcg();
    let mut dcg = init_naive();

    let mut naive_input: Trie<usize> = SetIntro::empty();
    let mut dcg_input: Trie<usize> = SetIntro::empty();

    let mut v = Vec::new();

    for i in 1..20 {
        assert!(engine_is_naive());
        v.push(i);
        naive_input = push_input(i, naive_input.clone());
        let naive_out = trie_fold(naive_input.clone(),
                                0,
                                Rc::new(|i_, acc| i_ + acc));

        use_engine(dcg);
        assert!(engine_is_dcg());
        dcg_input = push_input(i, dcg_input.clone());
        let dcg_out = trie_fold(dcg_input.clone(),
                                0,
                                Rc::new(|i_, acc| i_ + acc));

        assert_eq!(naive_out, dcg_out);
        assert_eq!(naive_out, v.iter().sum());
        dcg = init_naive();
    }
}
