use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use std::path::Path;

use macros::*;
use collections::*;
use engine::*;

//extern crate time;
//extern crate csv;

#[derive(Clone,Copy,Hash,Eq,PartialEq,Debug)]
pub enum Op { Plus, Minus, Times, Divide, ParenOpen, ParenClose }

#[derive(Clone,Copy,Hash,Eq,PartialEq,Debug)]
pub enum Tok {
  Num(isize),
  Op(Op)
}    

pub enum Precedence { Higher, Lower, Equal }

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

pub fn precedence_check (top_op:&Op, next_op:&Op) -> Precedence {
  match (*top_op, *next_op) {
    (Op::Plus, Op::Plus) => Precedence::Equal,
    (Op::Plus, Op::Minus) => Precedence::Equal,
    (Op::Minus, Op::Plus) => Precedence::Equal,
    (Op::Minus, Op::Minus) => Precedence::Equal,

    (Op::Times, Op::Times) => Precedence::Equal,
    (Op::Divide, Op::Divide) => Precedence::Equal,

    (Op::Times, Op::Plus)   => Precedence::Higher,
    (Op::Times, Op::Minus)  => Precedence::Higher,
    (Op::Times, Op::Divide) => Precedence::Higher,

    (Op::Divide, Op::Plus)  => Precedence::Higher,
    (Op::Divide, Op::Minus) => Precedence::Higher,
    (Op::Divide, Op::Times) => Precedence::Higher,
    (_, Op::ParenOpen) => Precedence::Higher,
    _ => Precedence::Lower
  }
}

pub fn check_ops_rec (ops:List<Op>, op:Op) -> bool {
  if List::is_empty(&ops) { true }
  else {
    let (top_op, popped_ops) = pop(ops);
    match op {
      Op::ParenOpen => {
	check_ops_rec(popped_ops, top_op)
        },
      _ => {match precedence_check(&op, &top_op) {
        Precedence::Higher => check_ops_rec(popped_ops, top_op),
        _ => false
      }}
    }
  }
}

pub fn check_ops (ops:List<Op>) -> bool {
  if List::is_empty(&ops) { true }
  else {
    let (top_op, popped_ops) = pop(ops);
    check_ops_rec(popped_ops, top_op)
  }
}

pub fn tok_of_char(input: Tree<char>) -> Tree<Tok> {
  fn num_of_digits_rec(num: isize, digits: List<char>) -> (Tok, List<char>) {
    if List::is_empty(&digits) { 
      (Tok::Num(num), digits) } else {
      let (top_dig, popped_digs) = pop(digits);
      let top_dig_as_isize = top_dig as isize - '0' as isize;
      if num > 0 {
           num_of_digits_rec(num + 10 * top_dig_as_isize, popped_digs) }
      else{
           num_of_digits_rec(top_dig_as_isize, popped_digs) }
    }
  };

  fn num_of_digits(digits: List<char>, output:List<Tok>) -> (List<char>, List<Tok>) {
    if List::is_empty(&digits) {(digits, output)}
    else { 
	let (num, digits) = num_of_digits_rec(0, digits);
	(digits, push(output, num)) }    
  };

  fn transfer_num(digits: List<char>, output: List<Tok>) -> (List<char>, List<Tok>) {
        let (digits, output) = num_of_digits(digits, output);
        (digits, output)
  };


  fn read_op(digits: List<char>, output: List<Tok>, op: Op) -> (List<char>, List<Tok>) {
	let (digits, output) = transfer_num(digits, output);
	(digits, push(output, Tok::Op(op)))
  };
  
  let (digits, output): (List<char>, List<Tok>) =
    tree_fold_seq 
      (input, Dir2::Left, (List::Nil, List::Nil),
      Rc::new(|ch, (digits, output) : (List<char>, List<Tok>)| {
         match ch {
	 	'+' => {read_op(digits, output, Op::Plus)},
                '-' => {read_op(digits, output, Op::Minus)},
                '*' => {read_op(digits, output, Op::Times)},
                '/' => {read_op(digits, output, Op::Divide)},
                '(' => {read_op(digits, output, Op::ParenOpen)},
                ')' => {read_op(digits, output, Op::ParenClose)},
		'0' => {(push(digits, '0'), output)},
		'1' => {(push(digits, '1'), output)},
		'2' => {(push(digits, '2'), output)},
		'3' => {(push(digits, '3'), output)},
		'4' => {(push(digits, '4'), output)},
		'5' => {(push(digits, '5'), output)},
		'6' => {(push(digits, '6'), output)},
		'7' => {(push(digits, '7'), output)},
		'8' => {(push(digits, '8'), output)},
		'9' => {(push(digits, '9'), output)},
		_ => {panic!()},
	 }
      }), 
      Rc::new(|_, a| a),
      Rc::new(|n:Name,_, (digs, toks)| {
        let toks = <List<Tok> as ListIntro<Tok>>::art(cell(n.clone(), toks));
        let toks = <List<Tok> as ListIntro<Tok>>::name(name_pair(name_unit(), n), toks); // Toggle this line to make things crash!
        (digs,toks) })
      );

  let (digits, output): (List<char>, List<Tok>)= transfer_num(digits, output);
  assert!(List::is_empty(&digits));
  let out  : Tree<Tok> = tree_of_list(Dir2::Right, output);
  (out)
}

pub fn postfix_of_infix(infix: Tree<Tok>) -> List<Tok> {
  let (ops, postfix) : (List<Op>, List<Tok>) =
    tree_fold_seq
    (infix, Dir2::Left, (List::Nil, List::Nil),
     Rc::new(|tok, (ops, postfix) : (List<Op>, List<Tok>)| {
       assert!(check_ops(ops.clone()));
       match tok {
         Tok::Num(n) => (ops, push(postfix, Tok::Num(n))),
         Tok::Op(op) => {
           match op { 
	    Op::ParenOpen => {(push(ops, op), postfix)}
            Op::ParenClose => {
              fn myloop (op:Op, ops:List<Op>, postfix:List<Tok>) -> (List<Op>, List<Tok>) {
                let (top_op, popped_ops) = pop(ops);
		match top_op {
		  Op::ParenOpen => {(popped_ops, postfix)},
                  _ => {myloop(op, popped_ops, push(postfix, Tok::Op(top_op)))},
                }
	      };
              myloop(op, ops, postfix)
	    }
	    _ => {
             if List::is_empty(&ops) { (push(ops, op), postfix) }
             else {
               fn myloop (op:Op, ops:List<Op>, postfix:List<Tok>) -> (List<Op>, List<Tok>) {
                 if List::is_empty(&ops) { (ops, postfix) } else {
                   let (top_op, popped_ops) = pop(ops);               
                   match precedence_check (&top_op, &op) {
                     Precedence::Higher => { myloop (op, popped_ops, push(postfix, Tok::Op(top_op)))},
	             Precedence::Equal => {(popped_ops, push(postfix, Tok::Op(top_op)))},
                     _ => {(push(popped_ops, top_op), postfix)}
                   }
                 }};
               let (ops, postfix) = myloop(op, ops, postfix) ;
               (push(ops, op), postfix)
             }
           }}
         }       
       }}),
     Rc::new(|_, a|   a),
     Rc::new(|n:Name,_, (ops, toks)| {
       let (n1, n2) = name_fork(name_pair(name_unit(), n.clone()));
       let ops  = <List<Op> as ListIntro<Op>>::art(cell(n1, ops));
       //let ops  = <List<Op> as ListIntro<Op>>::name(n.clone(), ops);
       let toks = <List<Tok> as ListIntro<Tok>>::art(cell(n2, toks));
       let toks = <List<Tok> as ListIntro<Tok>>::name(n.clone(), toks); // Including name n in toks output; important!
       (ops, toks)}
     ),
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


pub fn evaluate_postfix(input: Tree<Tok>) -> isize {
  let stack =
    tree_fold_seq
    (input, Dir2::Left, List::Nil,                  
     Rc::new(|tok, stack| {
       match tok {
         Tok::Op(op) => {
           match op {
             Op::ParenClose => {panic!()},
             Op::ParenOpen => {panic!()},
             _ => {   
               let (x,stack) = pop(stack);
               let (y,stack) = pop(stack);
               let z = match op {
                 Op::Plus   => y + x,
                 Op::Minus  => y - x,
                 Op::Times  => y * x,
                 Op::Divide => y / x,
                 _ => panic!(),
               };
               push(stack, z)
            }
          }
         },
         Tok::Num(n) => {
           push(stack, n)
         }
       }}),
     Rc::new(|_, stack|   stack),
     Rc::new(|n,_, stack|{
       let stack = <List<isize> as ListIntro<isize>>::art(cell(n, stack));
       stack}),
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
    NameElse::Else( Tok::Op(Op::Minus) ),
    NameElse::Else( Tok::Num(1) ),
    NameElse::Else( Tok::Op(Op::Minus) ),    
    NameElse::Else( Tok::Num(2) ),
    NameElse::Else( Tok::Op(Op::Times) ),
    NameElse::Else( Tok::Num(2) ),
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

#[test]
fn test_paren_eval () {

  // 1 * 2 + 3
  let input = vec![
    NameElse::Else( Tok::Num(2) ),
    NameElse::Else( Tok::Op(Op::Times) ),
    NameElse::Else( Tok::Op(Op::ParenOpen) ),
    //NameElse::Else( Tok::Num(1) ),
    //NameElse::Else( Tok::Op(Op::Plus) ),
    NameElse::Else( Tok::Num(23) ),
    NameElse::Else( Tok::Op(Op::Minus) ),
    NameElse::Else( Tok::Num(3) ),
    NameElse::Else( Tok::Op(Op::ParenClose) ),
    ];
 
    println!("{:?}", &input);
    let input : List<Tok> = list_of_vec(&input);
    let tree  : Tree<Tok> = tree_of_list(Dir2::Left, input);
    println!("Tree {:?}", &tree);
    let list  : List<Tok> = postfix_of_infix(tree);
    println!("Postfix{:?}", &list);
    //let input : List<Tok> = list_of_vec(&input);
    let tree  : Tree<Tok> = tree_of_list(Dir2::Right, list);
    let ans = evaluate_postfix(tree);
    println!("{:?}", ans)
}

#[test]
fn test_tok_of_char() {
   let input = vec![
   NameElse::Else('2'),
   NameElse::Else('*'),
   NameElse::Else('('),
   NameElse::Else('2'),
   NameElse::Else('3'),
   NameElse::Else('-'),
   NameElse::Else('3'),
   NameElse::Else(')'),];
   let input : List<char> = list_of_vec(&input);
   let tree  : Tree<char> = tree_of_list(Dir2::Left, input);
   let list  : Tree<Tok> = tok_of_char(tree);
   let list1  : List<Tok> = postfix_of_infix(list);
   let tree  : Tree<Tok> = tree_of_list(Dir2::Right, list1);
   let ans = evaluate_postfix(tree);
   println!("Ans {:?}", ans)

}

// Removed this one-off test system: Use Adapton Lab instead:

// pub fn runtime_harness(max_len: isize) -> Vec<(isize, u64, isize)> {

//   let mut runtimes: Vec<(isize, u64, isize)> = vec![];
//   //fn construct_exp(len: isize) -> Vec<NameElse<char>>{
//   //  let cap = len as _ ;
//   //  let mut input = Vec::with_capacity(cap);
//   //  for _ in 0..len {
//   //    input.push(NameElse::Else('1'));
//   //    input.push(NameElse::Else('+'));
//   //  }
//   //  input.push(NameElse::Else('1'));
//   //  input
//   //};

//   fn push_char(name_step:usize, name:usize, ch: char, l:List<char>) -> List<char>{
//     let l = if name % name_step == 0 {
//       let l = <List<char> as ListIntro<char>>::art(cell(name_of_usize(name), l));
//       let l = <List<char> as ListIntro<char>>::name(name_of_usize(name), l);
//       l
//     } else { l } ;
//     let l = <List<char> as ListIntro<char>>::cons(ch, l);
//     l
//   };

//   fn construct_input(len: isize) -> List<char> {
//     let mut input: List<char> = List::Nil;
//     for _ in 0..len {
//       input = push(input.clone(), '1');
//       input = push(input.clone(), '+');
//     }
//     input = push(input.clone(), '1');
//     input
//   };

//   fn prepend_input(input:List<char>, name:isize) -> List<char> {
//     let input = <List<char> as ListIntro<char>>::art(cell(name_of_isize(name), input));
//     let input = <List<char> as ListIntro<char>>::name(name_of_isize(name), input);
//     let input = push(input.clone(), '+');
//     let input = push(input.clone(), '1');
//     input
//   };

//   fn doit(input: Art<List<char>>) -> isize {
//     //let list : List<char>  = list_of_vec(&input);
//     let input = force(&input);
//     let tree : Tree<char> = 
//       ns(name_of_str("tree_of_list"),
//          ||tree_of_list(Dir2::Left, input));
    
//     // TODO: How to use names here?
//     let tokenized_input : Tree<Tok> = 
//       ns(name_of_str("tok_of_char"),
//          ||tok_of_char(tree));
//     // TODO: How to use names here?    
//     let postfix : List<Tok> = 
//       ns(name_of_str("postfix_of_infix"),
//          ||postfix_of_infix(tokenized_input));
    
//     let postfix_tree : Tree<Tok> = 
//       ns(name_of_str("tree_of_list2"),
//          ||tree_of_list(Dir2::Right, postfix));	
    
//     // TODO: How to use names here?
//     ns(name_of_str("evaluate_postfix"),
//        ||evaluate_postfix(postfix_tree))
//   };

//   fn csv_of_runtimes(path:&str, runtimes: Vec<(isize, u64, isize)>) {
//     let path = Path::new(path);
//     let mut writer = csv::Writer::from_file(path).unwrap();
//     for r in runtimes.into_iter() {
//       //println!("{:?}",r);
//       writer.encode(r).ok().expect("CSV writer error");
//     }
//   };

//   { // This should be really fast:
//     init_dcg();
//     assert!(engine_is_dcg());
    
//     let mut input : List<char> = List::Nil;
//     input = push(input, '1');
//     for i in 1..max_len {
//         input = prepend_input(input, i);
//         let input = input.clone();
//         let dcg_start = time::precise_time_ns();
//         let input_cell = cell(name_of_str("input"), input);
//         let (_, dcg_out) = eager!(doit, input:input_cell);
//         let dcg_end = time::precise_time_ns();
//         println!("DCG: {}, {} ms, {}", i, (dcg_end - dcg_start) as u64 / 1000000, dcg_out);
//         runtimes.push((i,dcg_end - dcg_start, dcg_out));
//     }
//       csv_of_runtimes("./dcg.csv", runtimes.clone()); 
//       //runtimes
//   }

//   if false 
//   { // This is really, really slow:
//     init_naive();
//     assert!(engine_is_naive());
    
//     let mut input : List<char> = List::Nil;
//     input = push(input, '1');
//     for i in 1..max_len {
//       input = prepend_input(input, i);
//       let naive_start = time::precise_time_ns();
//       let naive_out = doit(cell(name_of_str("input"), input.clone()));
//       let naive_end = time::precise_time_ns();
//       //println!("Naive: {}, {}, {}", i, naive_end - naive_start, naive_out);
//       runtimes.push((i,naive_end - naive_start, naive_out));}
//     csv_of_runtimes("./naive.csv", runtimes.clone()); 
//     //runtimes
//   } else {}
//   Vec::new()
// }

// #[test]
// fn test_runtime_harness() {
//   use std::thread;
//   let child =
//     thread::Builder::new().stack_size(64 * 1024 * 1024).spawn(move || { 
//       runtime_harness(16);
//     });
//   let _ = child.unwrap().join();
// }

pub fn generate_balanced_string () {
  use std::hash::{Hash,Hasher};
  use std::collections::hash_map::DefaultHasher;
  
  fn my_hash<T>(obj: T) -> u64 where T: Hash {
    let mut hasher = DefaultHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
  };
    
  fn level<T>(x:T) -> usize where T: Hash {
    let h = my_hash(x);
    h.trailing_zeros() as usize
  };

  let (oparen,cparen) = ('(',')');
  let mut out : Vec<char> = vec![ ];
  let mut opc = 0; // open paren count
  out.push('1');
  for i in 0..100 {
    let lev = level(i);    
    println!("{:?} {:?}", i, lev);
    while lev != opc {
      if lev > opc {
        out.push('+');
        out.push(oparen);
        out.push('1');
        opc += 1;
      }       
      else {
        out.push('+');
        out.push('1');
        out.push(cparen);
        opc -= 1;
      }
    }    
  }
  let outs: String = out.into_iter().collect();
  //println!("{:?}", outs);
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

