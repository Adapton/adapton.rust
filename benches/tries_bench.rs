#![feature(test)]
extern crate adapton;
extern crate test;
use self::test::Bencher;
use adapton::trie::*;
use adapton::engine::*;

// The code that we want to compare/measure under naive versus DCG engines:
fn doit(v: Vec<usize>, t: Trie<usize>) -> Trie<usize> {
    v.into_iter().fold(t, |acc, i| TrieIntro::extend(name_of_usize(i), acc, i))
}

fn bench_naive_add_dups() {
    init_naive();
    let mut naive_input: Trie<usize> = SetIntro::empty();
    let mut v = Vec::new();

    for i in vec![1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3].iter() {
        v.push(*i);
        naive_input = doit(v.clone(), naive_input.clone())
    }
}

fn bench_dcg_add_dups() {
    init_dcg();
    let mut dcg_input: Trie<usize> = SetIntro::empty();
    let mut v = Vec::new();

    for i in vec![1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3].iter() {
        v.push(*i);
        dcg_input = doit(v.clone(), dcg_input.clone())
    }
}

#[bench]
fn benchmark_naive_trie(b: &mut Bencher) {
    b.bench_n(1, |b| b.iter(|| bench_naive_add_dups()))
}

#[bench]
fn benchmark_dcg_trie(b: &mut Bencher) {
    b.bench_n(1, |b| b.iter(|| bench_dcg_add_dups()))
}
