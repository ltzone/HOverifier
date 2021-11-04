
   type list = Nil
   | Cons of (int * list)
(* Based on the definition of list, the basic predicates for 
[self::Nil<>] datatype will be provided as [self = Nil] and 
[self::Cons<v,p>], where [v] is an integer and [p::list] *)

(*
should be encoded as
x::Nil() ~ NilP(x)
xs::Cons(x,xs') 
*)

(* type tree = Leaf
| Node of (int * tree * tree) *)

let rec fold_left f x ys = 
(*@ 
declare fold_left(f:int->int->int,x:int,ys:list)
given fpure(int,int,int)
requires {LL_foldl(x,r,ys,fpure) with f(x, y) |= {true} *->:r {fpure(x,y,r)}  }
ensures[res]  { LL_foldl(x,res,ys,fpure) }
@*)
match ys with
| Nil -> x
| Cons (y, ys') -> fold_left f (f x y) ys'




(* let sum_left xs = fold_left (+) 0 xs
(* Requires xs::LL<n,s>
Ensures  xs::LL<n,res> *) *)


(*@ 
  pred LL_foldl(x:int,r:int,ys:list,fpure:int->int->int->bool)
  |= ys::Nil<> & x=r
  or EX (ys':list) (y:int), ys::Cons<y,ys'> & LL_foldl(z,r,ys',fpure) & fpure(x,y,z) 

  pred LL_len(l:int,xs:list)
  |= xs::Nil<> & l=0
  or EX (xs':list) (x:int), xs::Cons<x,xs'> & LL_len(l - 1 , xs') 
@*)