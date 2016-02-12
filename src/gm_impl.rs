use std::fs::File;
use std::fmt::Debug;
use std::hash::Hash;

use time;
use gm;
use adapton::collection::{List, Tree};
use adapton::collection_edit::ListZipper;
use adapton::collection_traits::{TreeT, TreeListT};
use adapton::adapton_sigs::Adapton;

impl<A:Adapton,E:Debug+Hash+PartialEq+Eq+Clone>
gm::GMLog<A> for List<A,E> {
  fn log_snapshot(self: &Self, st: &mut A, file: &mut File, msg: Option<&str>) {
    gm::startframe(file, &format!("List logged at {}", time::now().asctime()), msg);
    list_out(st, self, file, "list_root");
  }
}

impl<A:Adapton,E:Debug+Hash+PartialEq+Eq+Clone,T:TreeT<A,E>,L:TreeListT<A,E,T,List=List<A,E>>>
gm::GMLog<A> for ListZipper<A,E,T,L> {
  fn log_snapshot(self: &Self, st: &mut A, file: &mut File, msg: Option<&str>) {
    gm::startframe(file, &format!("Zipper logged at {}", time::now().asctime()), msg);
    zipper_out(st, self, file, "cursor");
  }
}

impl<A:Adapton,E:Debug+Hash+PartialEq+Eq+Clone,L:Hash+Debug+Eq+Clone>
gm::GMLog<A> for Tree<A,E,L> {
  fn log_snapshot(self: &Self, st: &mut A, file: &mut File, msg: Option<&str>) {
    gm::startframe(file, &format!("Tree logged at {}", time::now().asctime()), msg);
    tree_out(st, self, file, "tree_root", "c");
  }
}

// #[derive(Debug,PartialEq,Eq,Hash,Clone)]
// pub enum Tree<A:Adapton,X,Lev> {
//     Nil,
//     Leaf(X),
//     Bin(          Lev, Box<Tree<A,X,Lev>>, Box<Tree<A,X,Lev>> ),
//     Name(A::Name, Lev, Box<Tree<A,X,Lev>>, Box<Tree<A,X,Lev>> ),
//     Rc(                 Rc<Tree<A,X,Lev>>),
//     Art(               Art<Tree<A,X,Lev>, A::Loc>),
// }
 
fn tree_out<A:Adapton,E:Debug+Hash+PartialEq+Eq+Clone,L:Hash+Debug+Eq+Clone>
(st: &mut A, t: &Tree<A,E,L>, f: &mut File, up: &str, side: &str) {
  let leaf = "green";
  let bin = "yellow";
  let name = "orange";
  let rc = "yellow";
  let art = "blue";
  fn edge(f: &mut File, from: &str, to: &str) {
      gm::addedge(f, from, to, "", "black", "", None)
  }
  match *t {
    Tree::Nil => {}
    Tree::Leaf(ref d) => {
      let node = format!("{:?}<-{}-{}",d,side,up);
      gm::addnode(f, &node, leaf, "", None);
      edge(f, up, &node);
    }
    Tree::Bin(_, ref t1, ref t2) => {
      let node = format!("bin<-{}-{}",side,up);
      gm::addnode(f, &node, bin, "", None);
      edge(f, up, &node);
      tree_out(st, &**t1, f, &node, "l");
      tree_out(st, &**t2, f, &node, "r");
    }
    Tree::Name(ref n,_, ref t1, ref t2) => {
      let node = format!("{:?}<-{}-{}",n,side,up);
      gm::addnode(f, &node, name, "", None);
      edge(f, up, &node);
      tree_out(st, &**t1, f, &node, "l");
      tree_out(st, &**t2, f, &node, "r");          
    }
    Tree::Rc(ref t) => {
      let node = format!("rc<-{}-{}",side,up);
      gm::addnode(f, &node, rc, "", None);
      edge(f, up, &node);
      tree_out(st, &**t, f, &node, "c");
    }
    Tree::Art(ref a) => {
      let node = format!("{:?}<-{}-",a,side);
      let t = st.force(a);
      gm::addnode(f, &node, art, "", None);
      edge(f, up, &node);
      tree_out(st, &t, f, &node, "c");          
    }
  }
}

// #[derive(Debug,PartialEq,Eq,Hash,Clone)]
// pub enum List<A:Adapton,Elm> {
//     Nil,
//     Cons(Elm, Box<List<A,Elm>>),
//     Tree(Box<Tree<A,Elm,u32>>, Dir2, Box<List<A,Elm>>),
//     Rc(Rc<List<A,Elm>>),
//     Name(A::Name, Box<List<A,Elm>>),
//     Art(Art<List<A,Elm>, A::Loc>),
// }

fn list_out<A:Adapton,E:Debug+Hash+PartialEq+Eq+Clone>
(st: &mut A, t: &List<A,E>, f: &mut File, up: &str) {
  let cons = "green";
  let tree = "green";
  let name = "orange";
  let rc = "yellow";
  let art = "blue";
  fn edge(f: &mut File, from: &str, to: &str) {
      gm::addedge(f, from, to, "", "black", "", None)
  }
  match *t {
    List::Nil => {}
    List::Cons(ref e, ref t) => {
      //use std::hash::{Hasher,SipHasher};
      //let hasher = &mut SipHasher::new();
      //up.hash(hasher);
      //let hash = hasher.finish();
      let node = format!("{:?}::{}",e, up);
      gm::addnode(f, &node, cons, "", None);
      edge(f, up, &node);
      list_out(st, &**t, f, &node);
    }
    List::Tree(ref tr, _, ref t) => {
      let node = format!("root::{}", up);
      gm::addnode(f, &node, tree, "", None);
      edge(f, up, &node);
      list_out(st, &**t, f, &node);
      tree_out(st, &**tr, f, &node, "c");
    }
    List::Name(ref n, ref t) => {
      let node = format!("{:?}::{}", n,up);
      gm::addnode(f, &node, name, "", None);
      edge(f, up, &node);
      list_out(st, &**t, f, &node);
    }
    List::Rc(ref t) => {
      let node = format!("rc::{}",up);
      gm::addnode(f, &node, rc, "", None);
      edge(f, up, &node);
      list_out(st, &**t, f, &node);
    }
    List::Art(ref a) => {
      let node = format!("{:?}::", a);
      let t = st.force(a);
      gm::addnode(f, &node, art, "", None);
      edge(f, up, &node);
      list_out(st, &t, f, &node);          
    }
  }
}

// #[derive(Debug,Hash,PartialEq,Eq,Clone)]
// pub struct ListZipper<A:Adapton,X,T:TreeT<A,X>,L:TreeListT<A,X,T>> {
//     /// Elements to the left of the focus, nearest to furthest.
//     pub left: L::List,
//     /// Elements to the right of the focus, nearest to furthest.
//     pub right: L::List,

//     /// Todo-Later: This phantom field should *not* be needed, but is, due to rustc being confused (?).
//     /// It complains that the parameter T is not used, though it is (by the type parameter L's trait bound).
//     phantom:PhantomData<T::Tree>,
// }

fn zipper_out<A:Adapton,E:Debug+Hash+PartialEq+Eq+Clone,T:TreeT<A,E>,L:TreeListT<A,E,T,List=List<A,E>>>
(st: &mut A, z: &ListZipper<A,E,T,L>, f: &mut File, up: &str) {
  let node = format!("{:?}", up);
  gm::addnode(f, &node, "black", "", None);
  list_out(st, &z.left, f, &node);
  list_out(st, &z.right, f, &node);
}


