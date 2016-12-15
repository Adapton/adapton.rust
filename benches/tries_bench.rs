#![feature(test)]
extern crate adapton;
extern crate test;
use self::test::Bencher;
use adapton::trie::*;
use adapton::engine::*;

// The code that we want to compare/measure under naive versus DCG engines:
fn doit(v:Vec<usize>, t:Trie<usize>) -> Trie<usize> {
    v.into_iter().fold(t, |acc, i| SetIntro::add(acc, i))
}

#[bench]
fn bench_naive_add_dups (b: &mut Bencher) {
    let mut naive_input : Trie<usize> = SetIntro::empty();
    init_naive();
    let mut v = Vec::new();

    for i in vec![1,1,1,1,1,2,2,2,2,2,3,3,3,3,3].iter()
    {
        v.push(*i);
        b.iter(|| naive_input = doit(v.clone(), naive_input.clone()))
    }
    // println!("{:?}", naive_input)
}
#[bench]
fn bench_dcg_add_dups (b: &mut Bencher) {
    let mut dcg_input : Trie<usize> = SetIntro::empty();
    init_dcg();
    let mut v = Vec::new();

    for i in vec![1,1,1,1,1,2,2,2,2,2,3,3,3,3,3].iter()
    {
        v.push(*i);
        b.iter(|| dcg_input = doit(v.clone(), dcg_input.clone()))
    }
    // println!("{:?}", dcg_input)
}
