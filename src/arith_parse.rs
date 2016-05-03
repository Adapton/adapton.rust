use std::fmt::Debug;
use std::hash::Hash;
use std::io;
//mod funlist;
//use funlist::*;
use std::rc::Rc;

use collections::*;

#[derive(Clone,Copy,Hash,Eq,PartialEq,Debug)]
enum Op { Plus, Minus, Times, Divide }

#[derive(Clone,Copy,Hash,Eq,PartialEq,Debug)]
enum Tok {
  Num(isize),
  Op(Op)
}    

fn pop<X:Clone+Copy+Hash+Eq+PartialEq+Debug>
  (stack:List<X>) -> (X, List<X>) {
    List::elim_arg(stack, (),
                   |_,_|    panic!("cannot pop an empty stack"),
                   |h,t, _| (h, t),
                   |_,t, _| pop(t))                   
}

fn push<X:Clone+Copy+Hash+Eq+PartialEq+Debug>
  (stack:List<X>, elm:X) -> List<X>
{ 
  List::cons(elm, stack)
}

fn evaluate_postfix(input: Tree<Tok>) -> isize {
  let stack =
    tree_fold_seq
    (input, Dir2::Left, List::Nil,                  
     Rc::new(|tok, stack| {
       match tok {
         Tok::Op(op) => {
           let (x,stack) = pop(stack);
           let (y,stack) = pop(stack);
           let z = match op {
             Op::Plus => x + y,
             _ => unimplemented!()
           };
           push(stack, z)
         },
         Tok::Num(n) => {
           push(stack, n)
         }
       }}),
     Rc::new(|_, stack|   stack),
     Rc::new(|_,_, stack| stack),
     );
  let (x, stack) = pop(stack);
  assert!(List::is_empty(&stack));
  x
}

#[test]
fn test_eval_postfix () {

  // 1 2 3 + + 
  let input = vec![
    NameElse::Else( Tok::Num(1) ),
    NameElse::Else( Tok::Num(2) ),
    NameElse::Else( Tok::Num(3) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Num(2) ),
    NameElse::Else( Tok::Num(3) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Num(2) ),
    NameElse::Else( Tok::Num(3) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    ];
  let input : List<Tok> = list_of_vec(&input);
  let tree  : Tree<Tok> = tree_of_list(Dir2::Left, input);
  let ans = evaluate_postfix(tree);
  println!("{:?}", ans);
}

// fn infix_to_postfix(input: List<char>) -> List<List<char>> {
//   let mut postfix = List::Nil;
//   let mut operators = List::Nil;
//   let mut temp_operand = List::Nil;

//   //'''
//   // Match all chars in 'input'
//   //     If c is a digit, then push onto a Digit list temporarily until all digits in an operand are read (after which add the operand to the Postfix list)
//   //     If c is an operator, pop everything of higher predence and add it to postfix list, then add c to list of Operators
//   //'''
//   postfix
// }

// fn evaluate_postfix(input: List<List<char>>) -> i32 {

//   //'''
//   // Convert list to number and apply operation on two at a time
//   //'''

// }

// fn test_arith_parse() {
//   println!("Enter expression in infix format");
//   let mut input = String::new();
//   io::stdin().read_line(&mut input).ok().expect("Failed to parse input");
  
//   // '''
//   // Convert string to List<char> (reverse order)
//   // '''
//   let input_list = panic!("");
  
//   let postfix_expr = infix_to_postfix(input_list);
//   let result = evaluate_postfix(postfix_expr);
// }

