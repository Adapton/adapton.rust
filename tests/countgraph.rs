// #![feature(test)]

// #[macro_use] extern crate adapton ;
// extern crate test ;

// use std::rc::Rc;
// use adapton::engine::* ;
// use adapton::macros::* ;

// /// Direction Graph where each node has exactly two outgoing edges
// #[derive(Debug,PartialEq,Eq,Hash,Clone)]
// pub struct BiDiGraph {
//   id:usize,
//   lsucc:Art<BiDiGraph>,
//   rsucc:Art<BiDiGraph>,
// }

// pub fn count_forever (g:BiDiGraph) -> usize {
//   let cl = eager!(count_forever, g:force(&g.lsucc));
//   let cr = eager!(count_forever, g:force(&g.rsucc));
//   1 + cl.1 + cr.1
// }

// pub fn count_visit_cases (g:BiDiGraph) -> usize {
//   fn visit (g:BiDiGraph,
//             visit_flag:bool,
//             rec:&(Fn(BiDiGraph)-> usize)) -> usize {
//     if visit_flag { 0 } else {
//       let gl = force(&g.lsucc);
//       let gr = force(&g.rsucc);
//       match (g.id == gl.id, g.id == gr.id, gl.id == gr.id) {
//         (false, false, false) => 1 + rec(gl) + rec(gr),
//         (true,  false, false) => 1 + 0       + rec(gr),
//         (false, true,  false) => 1 + rec(gl) + 0,
//         (false, false, true)  => 1 + rec(gl) + 0,
//         (true,  true,  true)  => 1 + 0       + 0,
//         (true,  true,  false) => unreachable!(),
//         (true,  false, true)  => unreachable!(),
//         (false, true,  true)  => unreachable!(),
//       }
//     }
//   };
//   let t = thunk_codata(prog_pt!(stringify!(visit)),
//                        Rc::new(Box::new(visit)), g);
//   force(&t)
// }

// pub fn count_visit (g:BiDiGraph) -> usize {
//   fn visit (g:BiDiGraph,
//             visit_flag:bool,
//             rec:&(Fn(BiDiGraph)-> usize)) -> usize {
//     if visit_flag { 0 } else {
//       let gl = force(&g.lsucc);
//       let gr = force(&g.rsucc);
//       1 + rec(gl) + rec(gr)
//     }
//   };
//   let t = thunk_codata(prog_pt!(stringify!(visit)),
//                        Rc::new(Box::new(visit)), g);
//   force(&t)
// }

// pub fn count_visit_trip (g:BiDiGraph) -> usize {
//   fn visit (g:BiDiGraph,
//             visit_flag:bool, st:Trip,
//             rec:&(Fn(BiDiGraph,Trip)-> (usize,Trip))) -> (usize,Trip) {
//     if visit_flag {
//       (0, st)
//     } else {
//       let gl      = force(&g.lsucc);
//       let gr      = force(&g.rsucc);
//       let (cl,st) = rec(gl,st);
//       let (cr,st) = rec(gr,st);
//       (1 + cr + cl, st)
//     }
//   };
//   let t = thunk_codata2(prog_pt!(stringify!(visit)),
//                         Rc::new(Box::new(visit)), g);
//   force(&t)
// }

// pub fn bomb (_x:usize) -> BiDiGraph { panic!("BOMB!") }

// // graph_RGB:
// //
// //      R 
// //    /  \
// //   Y -- G
// //
// // Creates a directed K3 graph shown above, with nodes {R,G,B}.
// // Each node has two successors: The other two nodes in the set.
// // We show undirected edges above, due to limitations of ASCII.
// //
// pub fn graph_rgb () -> BiDiGraph {
//   let nr = name_of_str("r");
//   let ng = name_of_str("g");
//   let ny = name_of_str("y");
//   let ag : Art<BiDiGraph> = thunk!(ng.clone() =>> bomb, _x:0);
//   let ay : Art<BiDiGraph> = thunk!(ny.clone() =>> bomb, _x:0);  
//   let r  = BiDiGraph{id:0, lsucc:ay.clone(), rsucc:ag.clone()};
//   let ar = cell(nr, r.clone());  
//   let g  = BiDiGraph{id:1, lsucc:ar.clone(), rsucc:ay};
//   let ag = cell(ng, g);    
//   let y  = BiDiGraph{id:2, lsucc:ag, rsucc:ar};
//   let _  = cell(ny, y); 
//   return r
// }

// #[test]
// pub fn count_graph() {
  
  
// }
