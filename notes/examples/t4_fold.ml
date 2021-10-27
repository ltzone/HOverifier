
(* Our library defines the basic predicates of data structures,
   implements basic higher order functions and defines their 
   specifications using a class of predicates *)

type list = Nil
          | Cons of (int * list)
(* Based on the definition of list, the basic predicates for 
   [self::Nil<>] datatype will be provided as [self = Nil] and 
   [self::Cons<v,p>], where [v] is an integer and [p::list] *)

   
(* LL_foldl[fpure]<x,r> == self::Nil<> & x=r
                        or self::Cons<y,ys'> * ys'::LL_foldl[fpure]<z,r> & z=fpure(x,y) *)

let rec fold_left f x ys = 
  (*@ Requires xs::LL_foldl[fpure]<x,r> & (f x y |= {true} *-> {fpure(x,y)})
      Ensures  xs::LL_foldl[fpure]<x,res>  *)
  match ys with
  | Nil -> x
  | Cons (y, ys') -> fold_left f (f x y) ys'




let sum_left xs = fold_left (+) 0 xs
(*@ Requires xs::LL<n,s>
    Ensures  xs::LL<n,res> *)