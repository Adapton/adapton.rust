(* Balanced fixed-point loop. *)
module Loop ( Sem : sig
  type state
  val level : state -> int
  val step : state -> state option
end ) = struct

  type state = Sem.state
  type tree = Empty | Bin of tree * state * tree

  let rec work
      (ctxlev:int) (state:state)
      (left:tree) (leftlev:int)
      : state * tree
      =
    match Sem.step state with
    | None -> (state, left)
    | Some next ->
       (* Probabilistically-balanced recursion,
          results in a balanced binary tree of explored states. *)
       (* See the "sequence" representation from Bill Pugh's 1989 POPL paper. *)
      let nextlev = Sem.level(next) (* Count trailing bits in hash *) in
      if ctxlev >= nextlev && nextlev >= leftlev then
        let state', right = work nextlev next Empty (-1) in
        work ctxlev state' (Bin(left,next,right))  nextlev
      else
        (state, left)

  let compute (start:state) : state * tree =
    work max_int start (Bin(Empty,start,Empty)) (-1)

end

(* Balanced fixed-point loop for exploring a tree of states by
   applying a user-provided step function.
   The states must form a tree since we don't check for cycles below.
   The loop can easily be breadth-first or depth-first.
*)
module TreeSearch ( Sem : sig
  type state
  val level : state -> int
  val step : state -> state list
end ) = struct

  type state = Sem.state
  type tree = Empty | Bin of tree * state * tree

  let rec work
      (ctxlev:int) (queue:state list)
      (left:tree) (leftlev:int)
      : state list * tree
      =
    match queue with
    | [] -> ([], left)
    | state :: rest ->
      let statelev = Sem.level(state) in
      if ctxlev >= statelev && statelev >= leftlev then
        let queue =
           (* true = Depth first, false = breadth first. *)
          if true then (Sem.step state) @ rest
          else rest @ (Sem.step state)
        in
        let queue, right = work statelev queue Empty (-1) in
        work ctxlev queue (Bin(left,state,right)) statelev
      else
        (queue, left)

  let compute (start:state) : tree * state =
    let snd (work max_int [start] (Bin(Empty,start,Empty)) (-1))
end

(* Balanced fixed-point loop for exploring a (possibly cyclic) graph
   of states by applying a user-provided step function.

   The "seen set" below is a list.  We want to use a better set rep for
   efficiency. (such as a trie.)
*)
module GraphSearch ( Sem : sig
  type state
  val level : state -> int
  val step : state -> state list
end ) = struct

  type state = Sem.state
  type tree = Empty | Bin of tree * state * tree

  let rec work
      (seen:state list) (* TODO: <-- Use a trie-based set rep here. *)
      (ctxlev:int) (queue:state list)
      (left:tree) (leftlev:int)
      : (state list) * (state list) * tree
      =
    match queue with
    | [] -> (seen, [], left)
    | state :: rest ->
      let statelev = Sem.level(state) in
      if ctxlev >= statelev && statelev >= leftlev then
        let queue =
          (List.filter (fun s -> not (List.mem s seen))
             (Sem.step state))
          @ rest
        in
        let seen, queue, right = work seen statelev queue Empty (-1) in
        work seen ctxlev queue (Bin(left,state,right)) statelev
      else
        (seen, queue, left)

  let compute (start:state) : state list * tree =
    let seen, _, trace =
      work [start] max_int [start] (Bin(Empty,start,Empty)) (-1)
    in (seen, trace)

end



module ExpSem = struct

  let fix (type st) : (st -> st option) -> st -> st = 
    fun step start ->
      failwith "To Finish: Make the types line up with modules above:
      let module Fp = 
            Loop (struct type state = st 
                         let step = step)
      in 
      Loop.compute start
      "
  
  (* - - - - - - - - - - -  *)

  type value = Num of int
  type exp   = Value of value | Plus of exp * exp
  
  type focus_ctx   = EmpCtx | PlusCtx of focus_ctx * exp
  type focus_state = focus_ctx * exp 
 
  type reduce_trace = exp list
  type reduce_state = reduce_trace * exp

  let focus_step : focus_state -> focus_state option = 
    function   
    | (ctx, Plus(Value _, Value _)) -> None
    | (ctx, Value _)                -> None
    | (ctx, Plus(e1, e2))           -> Some (PlusCtx(ctx, e2), e1)

  let unfocus_step : focus_state -> focus_state option =
    function
    | (EmpCtx, _)            -> None
    | (PlusCtx(ctx, e2), e1) -> Some (ctx, Plus(e1, e2))

  let reduce_step : reduce_state -> reduce_state option =
    function
    | (_, Value _) -> None
    | (trace, exp) ->
      let (ctx, redex) = (fix focus_step) (EmpCtx, exp) in
      let reduced_redex = 
        match redex with
        | Plus(Value(Num(p)), Value(Num(q))) -> Value(Num(p+q))
        | _ -> failwith "impossible"
      in
      let (ctx, exp) = (fix unfocus_step) (ctx, reduced_redex) in
      assert ( ctx = EmpCtx ) ;
      Some (exp :: trace, exp)

  let reduce exp : exp -> exp list = 
    fun exp ->
      let tr,exp = ((fix reduce_step) ([], exp)) in
      (exp::tr)

end

