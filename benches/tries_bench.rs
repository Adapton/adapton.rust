#![feature(test)]
extern crate adapton;
extern crate test;
use self::test::Bencher;
use adapton::collections::{Dir2, List, ListIntro, Tree, monoid_of_tree, tree_of_list};
use adapton::trie::*;
use adapton::engine::*;
use std::rc::Rc;

mod add_dups {
    use super::*;

    // The code that we want to compare/measure under naive versus DCG engines:
    fn doit(v: Vec<usize>, t: Trie<usize>) -> Trie<usize> {
        v.into_iter().fold(t, |acc, i| {
            let t = Trie::art(cell(name_of_usize(i), acc));
            let t = Trie::name(name_of_usize(i), t);
            Trie::extend(name_unit(), t, i)
        })
    }

    fn bench_naive_add_dups() {
        init_naive();
        let mut naive_input: Trie<usize> = SetIntro::empty();
        let mut v = Vec::new();

        let mut ones = vec![1;50];
        let mut twos = vec![2;50];
        let mut threes = vec![3;50];
        twos.append(&mut threes);
        ones.append(&mut twos);
        for i in ones.iter() {
            v.push(*i);
            naive_input = doit(v.clone(), naive_input.clone());
            let _ = SetElim::mem(&naive_input, i);
        }
    }

    fn bench_dcg_add_dups() {
        init_dcg();
        let mut dcg_input: Trie<usize> = SetIntro::empty();
        let mut v = Vec::new();

        let mut ones = vec![1;50];
        let mut twos = vec![2;50];
        let mut threes = vec![3;50];
        twos.append(&mut threes);
        ones.append(&mut twos);
        for i in ones.iter() {
            v.push(*i);
            dcg_input = doit(v.clone(), dcg_input.clone());
            let _ = SetElim::mem(&dcg_input, i);
        }
    }

    #[bench]
    fn benchmark_naive_trie(b: &mut Bencher) {
        // b.iter(|| bench_naive_add_dups())
        init_naive();
        let mut naive_input: Trie<usize> = SetIntro::empty();
        let mut v = Vec::new();

        let mut ones = vec![1;50];
        let mut twos = vec![2;50];
        let mut threes = vec![3;50];
        twos.append(&mut threes);
        ones.append(&mut twos);
        for i in ones.iter() {
            v.push(*i);
            b.iter(|| {
                naive_input = doit(v.clone(), naive_input.clone());
                SetElim::mem(&naive_input, i)
            });
            naive_input = doit(v.clone(), naive_input.clone());
            // b.iter(|| SetElim::mem(&naive_input, i));
        }
    }

    #[bench]
    fn benchmark_dcg_trie(b: &mut Bencher) {
        // b.iter(|| bench_dcg_add_dups())
        init_dcg();
        let mut dcg_input: Trie<usize> = SetIntro::empty();
        let mut v = Vec::new();

        let mut ones = vec![1;50];
        let mut twos = vec![2;50];
        let mut threes = vec![3;50];
        twos.append(&mut threes);
        ones.append(&mut twos);
        for i in ones.iter() {
            v.push(*i);
            b.iter(|| {
                dcg_input = doit(v.clone(), dcg_input.clone());
                SetElim::mem(&dcg_input, i)
            });
            dcg_input = doit(v.clone(), dcg_input.clone());
            // b.iter(|| SetElim::mem(&dcg_input, i));
        }
    }
}

mod sum_fold {
    use super::*;

    // The code that we want to compare/measure under naive versus DCG engines:
    fn doit(t: Trie<usize>) -> usize {
        SetElim::fold(t, 0, Rc::new(|i, acc| i + acc))
    }

    fn push_input(i: usize, t: Trie<usize>) -> Trie<usize> {
        let t = Trie::art(cell(name_of_usize(i), t));
        let t = Trie::name(name_of_usize(i), t);
        Trie::extend(name_unit(), t, i)
    }

    fn bench_naive_fold() {
        init_naive();
        let mut naive_input: Trie<usize> = SetIntro::empty();

        for i in (1..100).into_iter() {
            naive_input = push_input(i, naive_input);
            let _ = doit(naive_input.clone());
        }
    }

    fn bench_dcg_fold() {
        init_dcg();
        let mut dcg_input: Trie<usize> = SetIntro::empty();

        for i in (1..100).into_iter() {
            dcg_input = push_input(i, dcg_input);
            let _ = doit(dcg_input.clone());
        }
    }

    #[bench]
    fn benchmark_naive_trie(b: &mut Bencher) {
        // b.iter(|| bench_naive_fold(b)
        init_naive();
        let mut naive_input: Trie<usize> = SetIntro::empty();

        for i in (1..100).into_iter() {
            naive_input = push_input(i, naive_input);
            b.iter(|| doit(naive_input.clone()))
        }
    }

    #[bench]
    fn benchmark_dcg_trie(b: &mut Bencher) {
        // b.iter(|| bench_dcg_fold())
        init_dcg();
        let mut dcg_input: Trie<usize> = SetIntro::empty();

        for i in (1..100).into_iter() {
            dcg_input = push_input(i, dcg_input);
            b.iter(|| doit(dcg_input.clone()))
        }
    }
}

mod tree_benchmarks {
    use super::*;

    fn sum_tree(l:List<usize>) -> usize {
        let t = ns(name_of_str("tree_of_list"),
                   ||tree_of_list::<_,_,Tree<_>,_>(Dir2::Left, l));
        ns(name_of_str("monoid_of_tree"),
           || monoid_of_tree(t, 0, Rc::new(|x, y| x + y)))
    }

    fn push_list(i: usize, l: List<usize>) -> List<usize> {
        let l = List::art(cell(name_of_usize(i), l));
        let l = List::name(name_of_usize(i), l);
        List::cons(i, l)
    }

    #[bench]
    fn benchmark_naive_tree(b: &mut Bencher) {
        init_naive();
        let mut naive_input: List<usize> = List::nil();

        for i in (1..100).into_iter() {
            naive_input = push_list(i, naive_input);
            b.iter(|| sum_tree(naive_input.clone()))
        }
    }

    #[bench]
    fn benchmark_dcg_tree(b: &mut Bencher) {
        init_dcg();
        let mut dcg_input: List<usize> = List::nil();

        for i in (1..100).into_iter() {
            dcg_input = push_list(i, dcg_input);
            b.iter(|| sum_tree(dcg_input.clone()))
        }
    }
}
