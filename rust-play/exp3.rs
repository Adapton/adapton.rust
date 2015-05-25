#![feature(unboxed_closures)]

#[derive(Show,Clone)]
pub enum Exp {
    Value(int),
    Plus(Box<Exp>,Box<Exp>),
}

#[derive(Show,Clone)]
pub enum Ctx {
    Emp,
    Plus(Box<Ctx>,Box<Exp>)
}

pub fn focus_step (ctx:Ctx,exp:Exp) -> Option<(Ctx,Exp)> {
    match exp {
        Exp::Value(_) => None,
        Exp::Plus(e1, e2) => Some((Ctx::Plus(box ctx,e2), *e1))
    }
}

pub fn unfocus_step (ctx:Ctx,exp:Exp) -> Option<(Ctx,Exp)> {
    match ctx {
        Ctx::Emp => None,
        Ctx::Plus(ctx, e2) => Some((*ctx, Exp::Plus(box exp, e2)))
    }
}

pub fn fix<T:Clone,F> (f:Box<F>, x:T) -> T where F : Fn<T,Option<T>> {
    let y = f.call(x.clone()) ; // TODO: Create a version of fix that does not clone
    match y {
        None => x,
        Some(y) => fix(f,y),
    }
}

pub fn focus (e:Exp) -> (Ctx,Exp) {
    fix (box focus_step, (Ctx::Emp,e))
}

pub fn unfocus (ctx:Ctx, e:Exp) -> Exp {
    let (ctx,exp) = fix (box unfocus_step, (ctx, e)) ;
    assert!( match ctx { Ctx::Emp => true, _ => false } );
    exp
}

pub fn redex_step (e:Exp) -> Option<Exp> {
    match e {
        Exp::Value(_) => None,
        Exp::Plus(_,_) => {
            let (ctx,redex) = focus(e);
            let reduced_redex = match redex {
                Exp::Plus(box Exp::Value(n),
                          box Exp::Value(m)) =>
                    Exp::Value(n + m),
                _ => panic!("impossible"),
            } ;
            let exp = unfocus (ctx,reduced_redex) ;
            Some(exp)
        }
    }
}

pub fn main () {

}
