(* Balanced fixed-point computation. *)
module Make ( Sem : sig 
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

  let compute (start:state) : tree = 
     snd (work max_int start (Bin(Empty,start,Empty)) (-1))
   
end
