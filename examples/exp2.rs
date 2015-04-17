use std::sync::Arc;

#[derive(Debug)]
pub enum Exp<'x> {
    Value(int),
    Plus(Box<Exp<'x>>, Box<Exp<'x>>),

    // Meta nodes:
    Arc(Arc<Exp<'x>>), // Arcs allow for (non-affine) sharing.
    StepsTo(Box<Exp<'x>>,Box<Exp<'x>>), // Expresses a StepsTo Relationship,
}

mod affine {
    use super::*;
    use std::sync::Arc;    
    
    pub fn step<'x> (e:Exp<'x>) -> (Exp<'x>, Option<Exp<'x>>) {
        match e {
            Exp::Value(v) => (Exp::Value(v), None),
            Exp::Plus(e1, e2) =>
                match *e1 {
                    Exp::Value(n) =>
                        match *e2 {
                            Exp::Value(m) => 
                                (Exp::Plus(box Exp::Value(n), box Exp::Value(m)),
                                 Some(Exp::Value(n+m))),
                            _ => {
                                let (e2, step_e2) = step(*e2) ;
                                match step_e2 {
                                    None => panic!("impossible"),
                                    Some(step_e2) => 
                                        (Exp::Plus(box Exp::Value(n), box e2),
                                         Some (Exp::Plus(box Exp::Value(n), box step_e2)))
                                }
                            }
                        },
                    
                    _ => {
                        let e1_ = step(*e1) ;
                        match e1_ {
                            (_, None) => panic!("impossible"),
                            (e1, Some(step_e1)) => {
                                let arc_e2 = Arc::new(*e2) ;
                                let e2_a = Exp::Arc(arc_e2.clone()) ;
                                let e2_b = Exp::Arc(arc_e2) ;
                                (Exp::Plus(box e1, box e2_a),
                                 Some(Exp::Plus(box step_e1, box e2_b)))
                            }
                        }
                    }
                },
            
            Exp::Arc(arc) => 
                // step(*arc) //~ERROR: cannot move out of dereference of `&`-pointer
                panic!("cannot implement this case"),

            Exp::StepsTo(one, two) => 
                match step(*two) {
                    (two, None) => (Exp::StepsTo(one, box two), None),
                    (two, Some(three)) => 
                        (Exp::StepsTo(one, box two), Some(three))
                },
        }
    }
}

fn step_loop<'x> ( stepcnt : int, exp : Exp<'x> ) -> Exp<'x> {
    let (exp,s) = affine::step(exp) ;
    match s {
        None => exp,
        Some(step_exp) => {
            println!("{}: {}\n --> {}\n", stepcnt, exp, step_exp) ;
            let step_full = Exp::StepsTo( box exp, box step_exp ) ;
            step_loop ( stepcnt+1, step_full )
        }
    }
}

pub fn test2 () {
    
    let e1 = Exp::Plus(box Exp::Plus(box Exp::Value(0),
                                     box Exp::Value(1)),
                       box Exp::Plus(box Exp::Value(2),
                                     box Exp::Plus(box Exp::Plus(box Exp::Value(0),
                                                                 box Exp::Value(3)),
                                                   box Exp::Plus(box Exp::Value(4),
                                                                 box Exp::Value(5))))) ;

    let e2 = Exp::Plus(box Exp::Value(1),
                       box Exp::Plus(box Exp::Value(2),
                                     box Exp::Plus(box Exp::Value(3),
                                                   box Exp::Plus(box Exp::Value(4),
                                                                 box Exp::Value(5))))) ;
    let e_trace = step_loop ( 0, e1 ) ;
    println!("Final trace: {}", e_trace)
}

pub fn main () {
    test2 ()
}
