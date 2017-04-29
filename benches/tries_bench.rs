#![feature(test)]
extern crate adapton;
extern crate test;
use self::test::Bencher;
use adapton::catalog::collections::{Dir2, List, ListIntro, Tree, monoid_of_tree, tree_of_list};
use adapton::catalog::collections::trie::*;
use adapton::engine::*;
use adapton::engine::manage::*;
use std::rc::Rc;

mod trie_input {
    use super::*;

    // The code that we want to compare/measure under naive versus DCG engines:
    fn doit(t: Trie<usize>) -> usize {
        trie_fold(t, 0, Rc::new(|i:usize, acc:usize| i + acc))
    }

    fn push_input(i: usize, t: Trie<usize>) -> Trie<usize> {
        let t = Trie::art(cell(name_of_usize(i), t));
        let t = Trie::name(name_of_usize(i), t);
        Trie::extend(name_unit(), t, i)
    }

    fn run_bench(b: &mut Bencher) {
        let mut input: Trie<usize> = SetIntro::empty();

        for i in (1..100).into_iter() {
            input = push_input(i, input);
            b.iter(|| doit(input.clone()))
        }
    }

    #[bench]
    fn benchmark_naive_trie(b: &mut Bencher) {
        init_naive();
        run_bench(b);
    }

    #[bench]
    fn benchmark_dcg_trie(b: &mut Bencher) {
        init_dcg();
        run_bench(b);
    }
}

mod tree_benchmarks {
    use super::*;

    fn sum_tree(t:Tree<usize>) -> usize {
        monoid_of_tree(t, 0, Rc::new(|x, y| x + y))
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
            let t = ns(name_of_str("tree_of_list"),
                       ||tree_of_list::<_,_,Tree<_>,_>(Dir2::Left, naive_input.clone()));
            b.iter(|| sum_tree(t.clone()))
        }
    }

    #[bench]
    fn benchmark_dcg_tree(b: &mut Bencher) {
        init_dcg();
        let mut dcg_input: List<usize> = List::nil();

        for i in (1..100).into_iter() {
            dcg_input = push_list(i, dcg_input);
            let t = ns(name_of_str("tree_of_list"),
                       ||tree_of_list::<_,_,Tree<_>,_>(Dir2::Left, dcg_input.clone()));
            b.iter(|| sum_tree(t.clone()))
        }
    }
}
