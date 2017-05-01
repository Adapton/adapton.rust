//! Parses the output of Rust `Debug` strings into reflected values of
//! type `Val`.  (See `engine::reflect` module).
//!
//! We use this parse as a non-intrusive mechanism for
//! building the values in the reflected DCG, which consists of
//! crawling user-defined data structures, and following their
//! articulations. We use the values' `Debug` strings to do this
//! traversal.

use std::fmt::Debug;
use adapton::reflect::{Loc,Path,Val,ArtContent,Const};
use adapton::engine::{Name, name_of_str, name_of_string};

/// _Balanced tokens_: Tokens that must be balanced with a left and
/// right instance and well-nested balanced tokens between them.
#[derive(Debug, Eq, PartialEq)]
enum BalTok {
    Paren, 
    Bracket, 
    Brace
}

/// _Tokens_: The unit of input for parsing Rust `Debug` strings
/// into reflected `Val` values.
#[derive(Debug, Eq, PartialEq)]
enum Tok {
    /// Left (and right) balanced tokens
    Left(BalTok), 
    /// Right (and left) balanced tokens
    Right(BalTok),
    /// Constant values that can immediately be injected into reflected `Val` type
    Const(Const),
    /// Identifers name fields of structs and constructors (of enums and structs)
    Ident(String),
    /// Colons separate field names from field values in structs.  Co
    Colon,
    /// Commas separate arguments to a constructor; for struct constructors, they separate fields
    Comma, 
}


/// Transform most(*) Rust data that derives `Debug` into a reflected
/// `Val`.
/// 
/// This parsing logic handles user-defined `struct` and `enum`
/// types, tuples and vectors.  It recognizes the `Debug` output of
/// these structures and parses them into trees of type
/// `Val`. Importantly, it recognizes articulations in this `Debug`
/// output and parses those into reflected locations (of type `Loc`)
/// and articulations (of type `Art`).  The reflected `DCG` maps
/// reflected articulations (whose reflected locations are of type
/// `Loc`) to reflected nodes that contain more reflected values.
/// 
/// (*) Note: Though this parsing logic handles vectors, tuples and
/// user-defined data types, this parsing logic is probably
/// incomplete for all of Rust's standard collections. (It does not
/// yet handle `HashMap<_,_>` debug output, or anything else of this
/// complexity as of yet).
pub fn parse_val <V:Debug> (v:&V) -> Val {
    let s = format!("{:?}", v);
    //println!("reflect_val({:?})", v);
    let toks = lex(s.into_bytes());
    //println!("toks = {:?}", toks);
    parse_toks(toks)
}

/// Tokenize the characters of input into lexical tokens of type `Tok`
fn lex (mut chars: Vec<u8>) -> Vec<Tok> {
    let mut toks = vec![];
    chars.reverse(); // TODO rewrite to avoid this
    loop {
        match chars.pop() {
            None => return toks,
            Some(c) => {
                let c : char = c as char ;
                if      c == ' ' { continue }
                else if c == ':' { toks.push(Tok::Colon); continue }
                else if c == ',' { toks.push(Tok::Comma); continue }
                else if c == '{' { toks.push(Tok::Left (BalTok::Brace));   continue }
                else if c == '[' { toks.push(Tok::Left (BalTok::Bracket)); continue }
                else if c == '(' { toks.push(Tok::Left (BalTok::Paren));   continue }
                else if c == '}' { toks.push(Tok::Right(BalTok::Brace));   continue }
                else if c == ']' { toks.push(Tok::Right(BalTok::Bracket)); continue }
                else if c == ')' { toks.push(Tok::Right(BalTok::Paren));   continue }
                else if c == '"' {
                    let mut string_chars = vec![];
                    loop {
                        match chars.pop() {
                            None => break,
                            Some(c) => {
                                let c : char = c as char ;
                                if c == '"' { break } else { 
                                    string_chars.push(c);
                                    continue 
                                }
                            }
                        }
                    };
                    toks.push(Tok::Const(Const::String( 
                        string_chars.into_iter().collect() 
                    )));
                    continue
                }
                else if c == '-' || (c >= '0' && c <= '9') {
                    let mut digs = vec![c];
                    loop {
                        match chars.pop() {
                            None    => break,
                            Some(c) => { 
                                let c : char = c as char ;
                                if c >= '0' && c <= '9' { 
                                    digs.push(c); 
                                    continue 
                                } else { 
                                    chars.push(c as u8); 
                                    break 
                                }
                            }
                        }
                    };
                    if c == '-' {
                        let s : String = digs.into_iter().collect();
                        toks.push(Tok::Const(Const::Num( 
                            isize::from_str_radix(s.as_str(), 10).unwrap()
                        )));              
                    } else {
                        let s : String = digs.into_iter().collect();
                        toks.push(Tok::Const(Const::Nat( 
                            usize::from_str_radix(s.as_str(), 10).unwrap()
                        )));
                    }
                    continue
                }
                else if (c >= 'a' && c <= 'z') ||
                    (c >= 'A' && c <= 'Z') ||
                    (c == '_') 
                {
                    let mut ident = vec![c];
                    loop {
                        match chars.pop() {
                            None    => break,
                            Some(c) => { 
                                let c : char = c as char ;
                                if (c >= 'a' && c <= 'z') ||
                                    (c >= 'A' && c <= 'Z') ||
                                    (c >= '0' && c <= '9') ||
                                    (c == '_')  
                                {
                                    ident.push(c); 
                                    continue 
                                } else { 
                                    chars.push(c as u8); 
                                    break 
                                }
                            }
                        }
                    };
                    toks.push(Tok::Ident( ident.into_iter().collect() ));
                    continue           
                }
            }
        }
    } ;
}

/// Parse a sequence of fields (appending to `fields`) until right
/// balanced token `bal`.  Return fields and remaining tokens.
fn parse_fields (mut toks:Vec<Tok>, mut fields:Vec<(Name, Val)>, bal:Tok) -> (Vec<(Name, Val)>, Vec<Tok>) {
    match toks.pop() {
        None => panic!("parse_vals: expected more vals, or end of sequence; but no more tokens"),
        Some(t) => {
            if t == bal { (fields, toks) } 
            else if t == Tok::Comma { 
                return parse_fields(toks, fields, bal)
            } else {
                match t {
                    Tok::Ident(i) => {
                        let toks = expect_tok(toks, Tok::Colon);
                        let (v, toks) = parse_val_rec(toks);
                        fields.push((name_of_string(i), v));
                        return parse_fields(toks, fields, bal)
                    }
                    t => {
                        panic!("parse_fields: expected identifier, but found {:?}", t)
                    }
                }
            }
        }
    }
}

/// Parse a sequence of values (appending to `vals`) until right
/// balanced token `bal`.  Return fields and remaining tokens.
fn parse_vals (mut toks:Vec<Tok>, mut vals:Vec<Val>, bal:Tok) -> (Vec<Val>, Vec<Tok>) {
    match toks.pop() {
        None => panic!("parse_vals: expected more vals, or end of sequence; but no more tokens"),
        Some(t) => {
            if t == bal { (vals, toks) } 
            else if t == Tok::Comma { 
                return parse_vals(toks, vals, bal)
            } 
            else {
                toks.push(t);
                let (v, toks) = parse_val_rec(toks);
                vals.push(v);
                return parse_vals(toks, vals, bal)
            }
        }
    }
}

/// Expect next token to be `tok` and panic otherwise.
fn expect_tok (mut toks: Vec<Tok>, tok:Tok) -> Vec<Tok> {
    match toks.pop() {
        None => panic!("expected token `{:?}`, but, no more tokens", tok),
        Some(t) => {
            if t == tok { toks } 
            else { panic!("expected token `{:?}`, but instead found token `{:?}`", tok, t) }
        }
    }
}

fn parse_toks(mut toks:Vec<Tok>) -> Val {
    toks.reverse();
    let (v, toks) = parse_val_rec(toks);
    assert!(toks.len() == 0);
    v
}

fn path_of_val ( p:&Val ) -> Path {
    match *p {
        Val::Vec( ref vs ) => vs.iter().map( name_of_val ).collect(),
        _ => panic!("expected a vector of values representing names"),
    }
}

fn name_of_val ( n:&Val ) -> Name {
    use engine::*;

    match *n {
        Val::Constr( ref cons_name, ref cons_args ) => {
            if *cons_name == name_of_str("Unit") {
                name_unit()
            }
            else if *cons_name == name_of_str("Hash64") {
                name_of_hash64( 0 ) // TODO/XXX
            }
            else if *cons_name == name_of_str("String") {
                name_of_string( match cons_args[0] {
                    Val::Const( Const::String( ref s ) ) => s.clone(),
                    _ => panic!("expected a String"),
                })
            }
            else if *cons_name == name_of_str("Usize") {
                name_of_usize( match cons_args[0] {
                    Val::Const( Const::Nat( ref n ) ) => n.clone(),
                    _ => panic!("expected a Nat"),
                })
            }
            else if *cons_name == name_of_str("Isize") {
                panic!("")
            }
            else if *cons_name == name_of_str("Pair") {
                let n1 = name_of_val( & cons_args[0] );
                let n2 = name_of_val( & cons_args[1] );
                name_pair(n1, n2)
            }
            else if *cons_name == name_of_str("ForkL") {
                let n = name_of_val( & cons_args[0] );
                name_fork(n).0
            }
            else if *cons_name == name_of_str("ForkR") {
                let n = name_of_val( & cons_args[0] );
                name_fork(n).1
            }
            else {
                unreachable!()
            }        
        },
        Val::Name(ref n) => n.clone(),
        _ => panic!("expected a constructor for a NameSym")
    }
}

fn name_option_of_val ( n:&Val ) -> Option<Name> {
    use engine::*;
    
    match *n {
        Val::Constr( ref cons_name, ref cons_args ) => {
            if *cons_name == name_of_str("Unit") {
                Some(name_unit())
            }
            else if *cons_name == name_of_str("Hash64") {
                Some(name_of_hash64( 0 )) // XXX \ TODO -- We are actually missing the hash here!
            }
            else if *cons_name == name_of_str("String") {
                if cons_args.len() < 1 { None } else {
                    match cons_args[0] {            
                        Val::Const( Const::String( ref s ) ) => 
                            Some(name_of_string( s.clone() )),
                        _ => None,
                    }}
            }
            else if *cons_name == name_of_str("Usize") {
                if cons_args.len() < 1 { None } else {
                    match cons_args[0] {          
                        Val::Const( Const::Nat( ref n ) ) => Some( name_of_usize( n.clone() ) ),
                        _ => None,
                    }}
            }
            else if *cons_name == name_of_str("Isize") {
                panic!("TODO")
            }
            else if *cons_name == name_of_str("Pair") {          
                if cons_args.len() < 2 { None } else {
                    let n1 = name_option_of_val( & cons_args[0] );
                    let n2 = name_option_of_val( & cons_args[1] );
                    if cons_args.len() < 2 { None } else {
                        match (n1,n2) {
                            (Some(n1),Some(n2)) => Some(name_pair(n1, n2)),
                            (_, _) => None,
                        }}}
            }
            else if *cons_name == name_of_str("ForkL") {
                if cons_args.len() < 1 { None } else {
                    let n = name_option_of_val( & cons_args[0] );
                    match n {
                        None => None,
                        Some(n) => Some(name_fork(n).0)
                    }}
            }
            else if *cons_name == name_of_str("ForkR") {
                if cons_args.len() < 1 { None } else {
                    let n = name_option_of_val( & cons_args[0] );
                    match n {
                        None => None,
                        Some(n) => Some(name_fork(n).1)
                    }}
            }
            else { None }
        },
        Val::Name(ref n) => Some(n.clone()),
        _ => None,
    }
}


/// Attempts to parse a reflected value into an `Art` value case,
/// which consists of parsing a location represented as a `Val`
/// structure into a `Loc` represented as a Rust data type (in the
/// `reflect` module).  If it fails to parse a value into an art, it
/// returns None.
fn parse_art_val ( i:&String, fields:&Vec<(Name, Val)> ) -> Option<Val> {
    if i == "Art" && fields.len() == 1 { 
        match fields[0] { 
            (ref nf, ref vf) =>
                if *nf == name_of_str("art") {
                    // OK: it's a struct called Art with exactly one field
                    // called art.  We are going to parse this into an Art.

                    match *vf { 
                        Val::Struct( ref j, ref ws ) => 
                            if *j == name_of_str("Loc") 
                            && ws.len() == 2
                            && ws[0].0 == name_of_str("path") 
                            && ws[1].0 == name_of_str("id")
                        {
                            // Now we are confident that the rest ought to parse.
                            // Any further parse errors are panics.
                            let path = path_of_val( & ws[0].1 );
                            let name = name_of_val( & ws[1].1 );
                            Some( Val::Art(Loc{path:path, name:name}, ArtContent::Unknown) )
                        } 


                        else { 
                            None 
                        },
                        _ => None,
                    }
                } else { None }
        }} else { None }           
}

/// Parse a value from the tokens `toks` and return it.  Panic if the next tokens do not parse into value.
fn parse_val_rec (mut toks:Vec<Tok>) -> (Val, Vec<Tok>) {
    //println!("{:?}", toks);
    let (v, toks) = match toks.pop() {
        None => panic!("expected value; but, no more tokens"),
        Some(Tok::Right(r)) => panic!("expected value, but found {:?} instead", Tok::Right(r)),
        Some(Tok::Comma) => panic!("expected value, but found Comma instead"),
        Some(Tok::Colon) => panic!("expected value, but found Colon instead"),
        Some(Tok::Left(BalTok::Bracket)) => {
            // Parse a vector: Begins with '[', then a list of comma-separated values, then ']'.
            let (vs, toks) = parse_vals(toks, vec![], Tok::Right(BalTok::Bracket));
            (Val::Vec(vs), toks)
        },
        Some(Tok::Left(BalTok::Paren)) => {
            // Parse a tuple: Begins with '(', then a list of comma-separated values, then ')'.
            let (vs, toks) = parse_vals(toks, vec![], Tok::Right(BalTok::Paren));
            (Val::Tuple(vs), toks)
        },
        Some(Tok::Left(l)) => panic!("expected value, but found {:?} instead", Tok::Left(l)),     
        Some(Tok::Ident(i)) => {
            match toks.pop() {
                None => {
                    // Constructors with no arguments (e.g., Nil)
                    (Val::Constr(name_of_string(i), vec![]), toks)
                }
                Some(Tok::Left(BalTok::Brace)) => {
                    //println!("parsing struct: {:?}", i);
                    let (fields, toks) = parse_fields(toks, vec![], Tok::Right(BalTok::Brace));
                    let art_op = parse_art_val(&i, &fields);
                    let v = match art_op {
                        Some(a) => a,
                        None => Val::Struct(name_of_string(i.clone()), fields.clone())
                    };    
                    (v, toks)
                }
                Some(Tok::Left(BalTok::Paren)) => {
                    //println!("parsing constructor: {:?}", i);
                    let (vs, toks) = parse_vals(toks, vec![], Tok::Right(BalTok::Paren));
                    let v = Val::Constr(name_of_string(i), vs);
                    match name_option_of_val(&v) {
                        Some(n) => (Val::Name(n), toks),
                        None => (v, toks)
                    }
                },
                Some(Tok::Comma) => {
                    toks.push(Tok::Comma);
                    (Val::Constr(name_of_string(i), vec![]), toks)
                },
                Some(Tok::Right(baltok)) => {
                    toks.push(Tok::Right(baltok));
                    (Val::Constr(name_of_string(i), vec![]), toks)
                },
                Some(t) => {
                    panic!("expected left balanced token, or comma, but instead found token {:?}", t)
                }}},
        Some(Tok::Const(Const::Nat(n)))    => (Val::Const(Const::Nat(n)), toks),
        Some(Tok::Const(Const::Num(n)))    => (Val::Const(Const::Num(n)), toks),
        Some(Tok::Const(Const::String(s))) => (Val::Const(Const::String(s)), toks)
    };
    (v,toks)
}
