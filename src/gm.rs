//TODO: implement styles
//TODO: return Results rather than panicing
//TODO: deal with empty strings

#![allow(dead_code)]

use std::io::prelude::*;
use std::fs::File;

use time;
use adapton::adapton_sigs::Adapton;
use adapton::engine::Engine;

pub const DEFAULT_STYLE: &'static str = "active";

pub trait GMLog<A:Adapton> {
  fn log_snapshot(self: &Self, st: &mut A, msg: Option<&str>) {
    startframe(st, &format!("Logged at {}", time::now().asctime()), msg)
  }
  fn log_comment(self: &Self, st:&mut A, msg: Option<&str>) {
    makecomment(st, &format!("Commented at {}", time::now().asctime()), msg)
  }
}

// pub trait GMAutoLog {
//   fn set_file(self: &mut Self, st: &mut File);
//   fn get_file(self: &Self) -> &mut File;
//   fn start(self: &Self, msg: Option<&str>);
//   fn snapshot(self: &Self, msg: Option<&str>);
//   fn comment(self: &Self, msg: Option<&str>);
//   fn end(self: &Self, msg: Option<&str>);
// }

trait CombineStr {
  fn join(self: &mut Self, sep: &str) -> String;
} 

impl<'a,I> CombineStr for I where I: Iterator<Item=&'a str> {
  fn join(self: &mut Self, sep: &str) -> String {
    let mut result: String = "".to_string();
    if let Some(part) = self.next() {
      result = result + part;
      for part in self {
        result = result + sep + part;
      }
    }
    result
  }
}

fn addtitle<A:Adapton>(st: &mut A, title: &str, comment: Option<&str>) {
  if let Some(file) = st.gmlog() {
    let title = title.lines().join(" ");
    writeln!(file, "{}", title).unwrap();
    if let Some(comment) = comment {
      let comment = comment.lines().join("\n ");
      writeln!(file, " {}", comment).unwrap();
    }
  }
}

pub fn startframe<A:Adapton>(st: &mut A, title: &str, comment: Option<&str>) {
  if let Some(file) = st.gmlog() {write!(file, "[state]").unwrap(); }
  addtitle(st, title, comment);
}

pub fn startdframe<A:Adapton>(st: &mut A, title: &str, comment: Option<&str>) {
  if let Some(file) = st.gmlog() { write!(file, "[change]").unwrap(); }
  addtitle(st, title, comment);
}

pub fn addnode<A:Adapton>(st: &mut A, id: &str, style: &str, name: &str, comment: Option<&str>) {
    let id = id.split("\"").join("").split_whitespace().join("_");
    let style = style.split("\"").join("").split_whitespace().join("_");
    if let Some(file) = st.gmlog() { write!(file, "[node {} {}]", id, style).unwrap(); }
    addtitle(st, name, comment);
}

pub fn addedge<A:Adapton>(st: &mut A, from: &str, to: &str, tag: &str, style: &str, name: &str, comment: Option<&str>) {
  let from = from.split("\"").join("").split_whitespace().join("_");
  let to = to.split("\"").join("").split_whitespace().join("_");
  let tag = tag.split("\"").join("").split_whitespace().join("_");
  let style = style.split("\"").join("").split_whitespace().join("_");
  if let Some(file) = st.gmlog() { write!(file, "[edge {} {} {} {}]", from, to, tag, style).unwrap(); }
  addtitle(st, name, comment);
}


pub fn makecomment<A:Adapton>(st: &mut A, short: &str, long: Option<&str>) {
  if let Some(file) = st.gmlog() {
    write!(file, "[comment]").unwrap();
  }
  addtitle(st, short, long);
}

// #[test]
// fn it_works() {
//   let f = &mut File::create("testlogging.txt").unwrap();
//   startframe(f, &"first".to_string(), None);
//   startdframe(f, "", Some("no title here?"));
//   startframe(f, "", None);
//   startdframe(f, "one\nmore", Some("then we're done"));
//   startframe(f, "this is it!", Some(" the
//     last
//     one!!!
//     "));
//   startdframe(f, "done", None);
//   addnode(f, "node 1", "blue", "a first node", None);
//   addedge(f, " node 1", "node2", "", "green", "", Some("a green line!\nbeat that ;)"));

//   impl<A:Adapton> GMLog<A> for String {};
//   let l: String = String::from("a logger");

//   let st = &mut Engine::new();
//   l.log_snapshot(st, f, Some("first snap"));
//   l.log_snapshot(st, f, None);
//   l.log_comment(st, f, Some("some words"));
// }
