type list = Nil
          | Cons of (int * list)

(* LL<n> == self::Nil & n=0
          or self::Cons<i,q> * q::LL<n-1>
    inv min<=max & n>=1  *)

(* LLsort<n,min,max> == self::Cons<min,q> * q::Nil & min=max & n=1
                     or self::Cons<min,q> * q::LLsort<n-1,k,max> & min <= k
    inv min<=max & n>0  *)

let rec sort lst =
(* Requires      lst::LL<n> & n>0
   Ensures[res]  res::LLsort<n,mi,ma> 

   Requires      lst::Nil
   Ensures[res]  res::Nil
*)
  match lst with
    Nil -> Nil
  | Cons (x, xs) -> insert x (sort xs)


and insert elt lst =
(* Requires      lst::LLsort<n,mi,ma>
   Ensures[res]  res::LLsort<n+1,min(elt,mi),max(elt,ma)>

  Requires      lst::Nil
  Ensures[res]  res::LLsort<1,elt,elt>
*)
  match lst with
    Nil -> Cons (elt, Nil)
  | Cons (x, xs) -> if elt <= x then Cons (elt, Cons(x, xs)) else Cons (x, insert elt xs)


