use std::fs::File;
use std::fmt::Debug;
use std::hash::Hash;

use time;
use gm;
use adapton::collection::{List, Tree};
use adapton::collection_edit::ListZipper;
use adapton::collection_traits::{TreeT, TreeListT};
use adapton::adapton_sigs::Adapton;

impl<A:Adapton,E:Debug+Hash+PartialEq+Eq+Clone,T:TreeT<A,E>,L:TreeListT<A,E,T,List=List<A,E>>>
gm::GMLog<A> for ListZipper<A,E,T,L> {
  fn log_snapshot(self: &Self, st: &mut A, msg: Option<&str>) {
    gm::startframe(st, &format!("Zipper logged at {}", time::now().asctime()), msg);
    zipper_out(st, self, "cursor");
  }
}

impl<A:Adapton,E:Debug+Hash+PartialEq+Eq+Clone>
gm::GMLog<A> for List<A,E> {
  fn log_snapshot(self: &Self, st: &mut A, msg: Option<&str>) {
    gm::startframe(st, &format!("List logged at {}", time::now().asctime()), msg);
    list_out(st, self, "list_root");
  }
}

impl<A:Adapton,E:Debug+Hash+PartialEq+Eq+Clone,L:Hash+Debug+Eq+Clone>
gm::GMLog<A> for Tree<A,E,L> {
  fn log_snapshot(self: &Self, st: &mut A, msg: Option<&str>) {
    gm::startframe(st, &format!("Tree logged at {}", time::now().asctime()), msg);
    tree_out(st, self, "tree_root", "c");
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
(st: &mut A, t: &Tree<A,E,L>, up: &str, side: &str) {
  let leaf = "green";
  let bin = "yellow";
  let name = "orange";
  let rc = "yellow";
  let art = "blue";
  fn edge<A:Adapton>(st: &mut A, from: &str, to: &str) {
    gm::addedge(st, from, to, "", "black", "", None)
  }
  match *t {
    Tree::Nil => {}
    Tree::Leaf(ref d) => {
      let node = format!("{:?}<-{}-{}",d,side,up);
      gm::addnode(st, &node, leaf, "", None);
      edge(st, up, &node);
    }
    Tree::Bin(_, ref t1, ref t2) => {
      let node = format!("bin<-{}-{}",side,up);
      gm::addnode(st, &node, bin, "", None);
      edge(st, up, &node);
      tree_out(st, &**t1, &node, "l");
      tree_out(st, &**t2, &node, "r");
    }
    Tree::Name(ref n,_, ref t1, ref t2) => {
      let node = format!("{:?}<-{}-{}",n,side,up);
      gm::addnode(st, &node, name, "", None);
      edge(st, up, &node);
      tree_out(st, &**t1, &node, "l");
      tree_out(st, &**t2, &node, "r");          
    }
    Tree::Rc(ref t) => {
      let node = format!("rc<-{}-{}",side,up);
      gm::addnode(st, &node, rc, "", None);
      edge(st, up, &node);
      tree_out(st, &**t, &node, "c");
    }
    Tree::Art(ref a) => {
      let node = format!("{:?}<-{}-",a,side);
      let t = st.force(a);
      gm::addnode(st, &node, art, "", None);
      edge(st, up, &node);
      tree_out(st, &t, &node, "c");          
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
(st: &mut A, t: &List<A,E>, up: &str) {
  let cons = "green";
  let tree = "green";
  let name = "orange";
  let rc = "yellow";
  let art = "blue";
  fn edge<A:Adapton>(st: &mut A, from: &str, to: &str) {
      gm::addedge(st, from, to, "", "black", "", None)
  }
  match *t {
    List::Nil => {}
    List::Cons(ref e, ref t) => {
      //use std::hash::{Hasher,SipHasher};
      //let hasher = &mut SipHasher::new();
      //up.hash(hasher);
      //let hash = hasher.finish();
      let node = format!("{:?}::{}",e, up);
      gm::addnode(st, &node, cons, "", None);
      edge(st, up, &node);
      list_out(st, &**t, &node);
    }
    List::Tree(ref tr, _, ref t) => {
      let node = format!("root::{}", up);
      gm::addnode(st, &node, tree, "", None);
      edge(st, up, &node);
      list_out(st, &**t, &node);
      tree_out(st, &**tr, &node, "c");
    }
    List::Name(ref n, ref t) => {
      let node = format!("{:?}::{}", n,up);
      gm::addnode(st, &node, name, "", None);
      edge(st, up, &node);
      list_out(st, &**t, &node);
    }
    List::Rc(ref t) => {
      let node = format!("rc::{}",up);
      gm::addnode(st, &node, rc, "", None);
      edge(st, up, &node);
      list_out(st, &**t, &node);
    }
    List::Art(ref a) => {
      let node = format!("{:?}::", a);
      let t = st.force(a);
      gm::addnode(st, &node, art, "", None);
      edge(st, up, &node);
      list_out(st, &t, &node);          
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
(st: &mut A, z: &ListZipper<A,E,T,L>, up: &str) {
  let node = format!("{:?}", up);
  gm::addnode(st, &node, "black", "", None);
  list_out(st, &z.left, &node);
  list_out(st, &z.right, &node);
}


