extern crate adapton;

use std::rc::Rc;
use adapton::bitstring::BS;
use adapton::engine::*;
use adapton::trie::*;

#[test]
fn test_is_empty() {
    let meta = Meta { min_depth: 1 };
    let empty = TrieIntro::<usize>::empty(meta.clone());
    let singleton = Trie::singleton(meta.clone(), name_unit(), 7);
    assert!(Trie::<usize>::is_empty(&Trie::nil(BS {
        length: 0,
        value: 0,
    })));
    assert!(Trie::is_empty(&empty));

    assert!(!Trie::is_empty(&Trie::leaf(BS {
                                            length: 0,
                                            value: 0,
                                        },
                                        0)));
    assert!(!Trie::is_empty(&singleton));
}

#[test]
fn test_equal() {
    let meta = Meta { min_depth: 1 };
    let empty: Trie<usize> = TrieIntro::empty(meta.clone());
    let singleton_7 = Trie::singleton(meta.clone(), name_unit(), 7);
    let singleton_7_ = Trie::singleton(meta.clone(), name_unit(), 7);
    let singleton_8 = Trie::singleton(meta.clone(), name_unit(), 8);
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
    let e: Set<usize> = SetIntro::empty();
    let t = push_input(8, e);
    let t = push_input(8, t);
    assert!(SetElim::mem(&t, &8));
    let t = push_input(7, t);
    let t = push_input(1, t);
    assert_eq!(SetElim::fold(t, 0, Rc::new(|i, acc| i + acc)), 16);
}

// The code that we want to compare/measure under naive versus DCG engines:
fn doit(v: Vec<usize>, t: Trie<usize>) -> Trie<usize> {
    v.into_iter().fold(t, |acc, i| {
        let t = Trie::art(cell(name_of_usize(i), acc));
        let t = Trie::name(name_of_usize(i), t);
        Trie::extend(name_unit(), t, i)
    })
}

#[test]
fn test_dcg_add_dups() {
    init_dcg();
    let mut dcg = init_naive();

    let mut naive_input: Trie<usize> = SetIntro::empty();
    let mut dcg_input: Trie<usize> = SetIntro::empty();

    let mut v = Vec::new();

    for i in vec![1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3].iter() {
        assert!(engine_is_naive());
        v.push(*i);
        naive_input = doit(v.clone(), naive_input.clone());
        let count_i = SetElim::fold(naive_input.clone(),
                                    0,
                                    Rc::new(|i_, acc| if *i == i_ { 1 } else { acc }));
        assert_eq!(count_i, 1);

        use_engine(dcg);
        assert!(engine_is_dcg());
        dcg_input = doit(v.clone(), dcg_input.clone());
        let count_i = SetElim::fold(dcg_input.clone(),
                                    0,
                                    Rc::new(|i_, acc| if *i == i_ { 1 } else { acc }));
        assert_eq!(count_i, 1);

        dcg = init_naive();
    }
}
