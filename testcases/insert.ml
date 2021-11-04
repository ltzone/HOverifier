type list = Nil
          | Cons of (int * list)

(*@ pred LL(n:int,l:list) |= l::Nil<> & n=0
          or EX (q:list), l::Cons<i,q> & LL( n - 1 , q)
    // inv min<=max & n>=1 
    @*)

(*@ pred LLsort(n,min,max,l:list) |= l::Cons<min,q> & q::Nil<> & min=max & n=1
                     or EX (q:list), l::Cons<min,q> & LLsort( n - 1 ,k,max,q) & min <= k
    // inv min<=max & n>0 
     @*)

let rec sort lst =
(*@ 
   declare sort(lst:list)
   requires      { LL(n,lst) &  0 <= n & ~ (0 = n) }
   ensures[res]   { EX (mi:int) (ma:int), LLsort(n,mi,ma,res) } 

//   Requires      lst::Nil
//   Ensures[res]  res::Nil
@*)
  match lst with
    Nil -> Nil
  | Cons (x, xs) -> insert x (sort xs)


and insert elt lst =
(* declare insert(elt:int,lst:list)
   requires      { LLsort(n,mi,ma,lst) }
   ensures[res]  { EX (mi_new:int) (ma_new:int), 
                    LLsort(n+1,mi_new,ma_new,res) &
                    mi_new <= mi & mi_new <= elt & elt <= ma_new & ma <= ma_new  }

//  Requires      lst::Nil
//  Ensures[res]  res::LLsort<1,elt,elt>
*)
  match lst with
    Nil -> Cons (elt, Nil)
  | Cons (x, xs) -> if elt <= x then Cons (elt, Cons(x, xs)) else Cons (x, insert elt xs)


