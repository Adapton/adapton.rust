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

fn precedence_check (top_op:&Op, next_op:&Op) -> bool {
  match (*top_op, *next_op) {
    (Op::Plus,  _       ) => false,
    (Op::Minus, Op::Plus) => true, // Critical!
    (Op::Minus, _       ) => false,

    (Op::Times, Op::Plus)   => true,
    (Op::Times, Op::Minus)  => true,
    (Op::Times, Op::Divide) => true,

    (Op::Divide, Op::Plus)  => true,
    (Op::Divide, Op::Minus) => true,
    (Op::Divide, Op::Times) => true,
    _ => false
  }
}

fn postfix_of_infix(infix: Tree<Tok>) -> List<Tok> {
  let (ops, postfix) : (List<Op>, List<Tok>) =
    tree_fold_seq
    (infix, Dir2::Left, (List::Nil, List::Nil),
     Rc::new(|tok, (ops, postfix)| {
       match tok {
         Tok::Num(n) => (ops, push(postfix, Tok::Num(n))),
         Tok::Op(op) => {
           if List::is_empty(&ops) { (push(ops, op), postfix) }
           else {
             fn myloop (op:Op, ops:List<Op>, postfix:List<Tok>) -> (List<Op>, List<Tok>) {
               if List::is_empty(&ops) { (ops, postfix) } else {
                 let (top_op, popped_ops) = pop(ops);               
                 if precedence_check (&top_op, &op) {
                   myloop (op, popped_ops, push(postfix, Tok::Op(top_op)))
                 }
                 else {
                   (push(popped_ops, top_op), postfix)
                 }
               }};
             let (ops, postfix) = myloop(op, ops, postfix) ;
             (push(ops, op), postfix)
           }
         }       
       }}),
     Rc::new(|_, a|   a),
     Rc::new(|_,_, a| a),
     );
  fn myloop (ops:List<Op>, postfix:List<Tok>) -> (List<Tok>) {
    if List::is_empty(&ops) { postfix } else {
      let (top_op, popped_ops) = pop(ops);      
      myloop (popped_ops, push(postfix, Tok::Op(top_op)))
    }};  
  myloop(ops, postfix)
}

#[test]
fn test_postfix_of_infix () {

  // 1 * 2 + 3
  let input = vec![
    NameElse::Else( Tok::Num(1) ),
    NameElse::Else( Tok::Op(Op::Times) ),
    NameElse::Else( Tok::Num(2) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Num(3) ),
    ];
  let input : List<Tok> = list_of_vec(&input);
  let tree  : Tree<Tok> = tree_of_list(Dir2::Left, input);
  let ans = postfix_of_infix(tree);
  println!("{:?}", ans)
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
             Op::Plus   => y + x,
             Op::Minus  => y - x,
             Op::Times  => y * x,
             Op::Divide => y / x,
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
  let input = vec![
    NameElse::Else( Tok::Num(1) ),
    NameElse::Else( Tok::Num(2) ),
    NameElse::Else( Tok::Num(3) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Num(22) ),
    NameElse::Else( Tok::Num(33) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Num(12) ),
    NameElse::Else( Tok::Num(13) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    ];
  let input : List<Tok> = list_of_vec(&input);
  let tree  : Tree<Tok> = tree_of_list(Dir2::Left, input);  
  let ans = evaluate_postfix(tree);
  assert_eq!(ans, 86)
}

#[test]
fn test_arith_eval () {

  // 1 * 2 + 3
  let input = vec![
    NameElse::Else( Tok::Num(1) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Num(2) ),
    NameElse::Else( Tok::Op(Op::Minus) ),
    NameElse::Else( Tok::Num(3) ),
    NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Num(1) ),
    // NameElse::Else( Tok::Op(Op::Plus) ),    
    // NameElse::Else( Tok::Num(1) ),
    // NameElse::Else( Tok::Op(Op::Times) ),
    // NameElse::Else( Tok::Num(2) ),
    // NameElse::Else( Tok::Op(Op::Divide) ),
    // NameElse::Else( Tok::Num(2) ),
    // NameElse::Else( Tok::Op(Op::Minus) ),
    // NameElse::Else( Tok::Num(3) ),
    ];
  println!("{:?}", &input);
  let input : List<Tok> = list_of_vec(&input);
  let tree  : Tree<Tok> = tree_of_list(Dir2::Left, input);
  let list  : List<Tok> = postfix_of_infix(tree);
  println!("{:?}", &list);
  //let input : List<Tok> = list_of_vec(&input);
  let tree  : Tree<Tok> = tree_of_list(Dir2::Right, list);
  let ans = evaluate_postfix(tree);
  println!("{:?}", ans)
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

