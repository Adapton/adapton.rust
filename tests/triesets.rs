extern crate adapton;

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
    println!("{:?}", s);
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

// The code that we want to compare/measure under naive versus DCG engines:
fn doit(v:Vec<usize>, t:Trie<usize>) -> Trie<usize> {
    let t_ = v.into_iter().fold(t, |acc, i| SetIntro::add(acc, i));
    println!();
    println!("adding...");
    println!("{:?}", t_);
    t_
}

#[test]
fn test_dcg_add_dups () {
    let mut naive_input : Trie<usize> = SetIntro::empty();
    let mut dcg_input : Trie<usize> = SetIntro::empty();
    println!("{:?}", naive_input);
    println!("{:?}", dcg_input);

    init_dcg();
    let mut dcg = init_naive();

    let mut v = Vec::new();

    for i in vec![1,1,1,1,1,2,2,2,2,2,3,3,3,3,3].iter()
    {
        assert!(engine_is_naive());
        v.push(*i);
        naive_input = doit(v.clone(), naive_input);
        let naive_output = naive_input.clone();

        use_engine(dcg);
        assert!(engine_is_dcg());
        dcg_input = doit(v.clone(), dcg_input);
        let dcg_output = dcg_input.clone();
        dcg = init_naive();


        assert_eq!(dcg_output, naive_output);
    }
}
