type list = Nil
| Cons of (int * list)
(* Based on the definition of list, the basic predicates for 
[self::list] datatype will be provided as [self = Nil] and 
[self::Cons<v,p>], where [v] is an integer and [p::list] *)

(* A simple list that only allows integer elements *)

(* 


*)


let rec map f = function
  | Nil -> Nil
  | Cons (x, xs') -> Cons (f x, map f xs')

let plus_one x = x + 1

let map_plus_one = map plus_one
