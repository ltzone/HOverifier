
type list = Nil
| Cons of (int * list)

(* pred LL<L> == self::Nil & L = []
              or self::Cons<x,xs> * self::LL<La> & L=x::La *)
(* pred LL_filter[fpure]<L1,L2> == self::Nil & L1 = [] & L2 = []
                                or self::Cons<x,xs> * self::LL_filter[fpure]<L1a,L2a>
                                 & L1 = x::L1a & (fpure(x) = true -> L2 = x::L2a)
                                 & (fpure(x) = false -> L2 = L2a) *)


let rec filter f = function
  | Nil -> Nil
  | Cons (x,xs) -> if f x then Cons(x, filter f xs) else filter f xs
(* Requires        f(x) |= {true} *->:r {r=fpure(x)}
   Ensures[resf]   resf(xs) |= { xs::LL_filter[fpure]<L,Lfiltered> } 
                                 *->:r { xs::LL_filter[fpure]<L,Lfiltered> * r::LL<Lfiltered> }
*)


(** IMPL issues
1. we might want to keep fpure separately so that users can define them explicitly when writing the assertion
   otherwise, if we keep them in the 



**)


(******** instances using filter **)
