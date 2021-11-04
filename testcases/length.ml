type list = Nil
          | Cons of (int * list)

(*@ pred LL(n:int,l:list) |= l::Nil<> & n=0
          or EX (q:list), l::Cons<i,q> & LL( n - 1 , q)
    @*)


let rec length lst =
(*@ 
   declare length(lst:list)
   requires      { LL(n,lst) }
   ensures[res]   { res = n } 

//   Requires      lst::Nil
//   Ensures[res]  res::Nil
@*)
  match lst with
    Nil -> 0
  | Cons (x, xs) -> 1 + length xs
