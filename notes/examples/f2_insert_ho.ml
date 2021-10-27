type list = Nil
          | Cons of (int * list)

(* LL<n> == self::Nil & n=0
          or self::Cons<i,q> * q::LL<n-1>
    inv min<=max & n>=1  *)

(* LLsort<n,min,max,cmpure> == self::Cons<min,q> * q::Nil & min=max & n=1
                     or self::Cons<min,q> * q::LLsort<n-1,k,max,cmpure> & cmpure(min, k) = true
    inv min<=max & n>0  *)

let rec sort cmp lst =
(* Requires      lst::LL<n> & n>0 & cmp(a,b) |= {true} *->:r {r=cmpure(a,b)} 
   Ensures[res]  res::LLsort<n,mi,ma,cmpure> 

   Requires      lst::Nil
   Ensures[res]  res::Nil
*)
  match lst with
    Nil -> Nil
  | Cons (x, xs) -> insert cmp x (sort cmp xs)


and insert cmp elt lst =
(* Requires      lst::LLsort<n,mi,ma,cmpure> & cmp(a,b) |= {true} *->:r {r=cmpure(a,b)} 
   Ensures[res]  res::LLsort<n+1,left,right,cmpure> 
                 & (cmpure(elt,mi) = true <-> left = elt )
                 & (cmpure(elt,mi) = false <-> left = mi )
                 & (cmpure(elt,ma) = true <-> right = ma )
                 & (cmpure(elt,ma) = false <-> left = elt )

  Requires      lst::Nil 
  Ensures[res]  res::Cons<elt,p> * p::Nil
*)
  match lst with
  | Nil -> Cons (elt, Nil)
  | Cons (mi, q) ->
    (* lst::LLsort<n,mi,ma,cmpure> & cmp(a,b) |= {true} *->:r {r=cmpure(a,b)}  *)
    (* [UNFOLD]
       self::Cons<mi,q> * q::Nil & mi=ma & n=1 
    or self::Cons<mi,q> * q::LLsort<n-1,k,ma> & cmpure(mi, k) = true 
  with cmp(a,b) |= {true} *->:r {r=cmpure(a,b)}  *)

    (* [APP] eval (cmp elt x) with anchor r1
      self::Cons<mi,q> * q::Nil & mi=ma & n=1 & r1 = cmpure(elt,mi)
    or self::Cons<mi,q> * q::LLsort<n-1,k,ma> & cmpure(mi, k) = true & r1 = cmpure(elt, mi)
    with cmp(a,b) |= {true} *->:r {r=cmpure(a,b)}  *)

    if (cmp elt mi)
    then
    (* [IF-1] 
        self::Cons<mi,q> * q::Nil & mi=ma & n=1 & r1 = cmpure(elt,mi) & r1 = true
    or  self::Cons<mi,q> * q::LLsort<n-1,k,ma> & cmpure(mi, k) = true & r1 = cmpure(elt, mi) & r1 = true
  with  cmp(a,b) |= {true} *->:r {r=cmpure(a,b)}  *)
    Cons (elt, Cons(mi, q))
  (* [VAL] with anchor r2, r3
        self::Cons<mi,q> * q::Nil & mi=ma & n=1 & r1 = cmpure(elt,mi) & r1 = true * r2::Cons<elt,r3> * r3::Cons(mi,q)
    or  self::Cons<mi,q> * q::LLsort<n-1,k,ma> & cmpure(mi, k) = true & r1 = cmpure(elt, mi) & r1 = true * r2::Cons<elt,r3> * r3::Cons<mi,q>
  with  cmp(a,b) |= {true} *->:r {r=cmpure(a,b)}  *)
  (* OR1
     [FOLD]
        self::Cons<mi,q> * q::Nil & mi=ma & n=1 & r1 = cmpure(elt,mi) & r1 = true * r2::Cons<elt,r3> * r3::LLsort<1,mi,mi,cmpure>
        |-- self::Cons<mi,q> * q::Nil & mi=ma & n=1 & r1 = cmpure(elt,mi) = true * r3::LLsort<2,elt,mi,cmpure>
     [SOLVE POST]
        ----------------------------- EX left = elt, right = ma
*** needs transitivity of cmpure { cmpure(x,y) = true -> cmpure(y,z) = true -> cmpure(x,z) = true }
*** needs invariant of predicate { cmpure(mi,ma) = true }
        |-- r3::LLsort<n+1,left,right,cmpure> 
            & (cmpure(elt,mi) = true <-> left = elt )
            & (cmpure(elt,mi) = false <-> left = mi )
            & (cmpure(elt,ma) = true <-> right = ma )
            & (cmpure(elt,ma) = false <-> left = elt ) *)
(* OR2
    or  self::Cons<mi,q> * q::LLsort<n-1,k,ma,cmpure> & cmpure(mi, k) = true & r1 = cmpure(elt, mi) & r1 = true * r2::Cons<elt,r3> * r3::Cons<mi,q>
  [FOLD]self::Cons<mi,q> * r3::LLsort<n-1+1,mi,ma,cmpure> * r2::Cons<elt,r3> & cmpure(mi, k) = true & r1 = cmpure(elt, mi) & r1 = true
  [FOLD]self::Cons<mi,q> * r3::LLsort<n+1,elt,ma,cmpure> & cmpure(mi, k) = true & r1 = cmpure(elt, mi) & r1 = true
  [SOLVE POST]
        ----------------------------- EX left = elt, right = ma
*** needs transitivity of cmpure { cmpure(x,y) = true -> cmpure(y,z) = true -> cmpure(x,z) = true }
*** needs invariant of predicate { cmpure(mi,ma) = true }
        |-- r3::LLsort<n+1,left,right,cmpure> 
            & (cmpure(elt,mi) = true <-> left = elt )
            & (cmpure(elt,mi) = false <-> left = mi )
            & (cmpure(elt,ma) = true <-> right = ma )
            & (cmpure(elt,ma) = false <-> left = elt ) *)
  else 
    (* [IF-1] 
        self::Cons<mi,q> * q::Nil & mi=ma & n=1 & r1 = cmpure(elt,mi) & r1 = false
    or  self::Cons<mi,q> * q::LLsort<n-1,k,ma> & cmpure(mi, k) = true & r1 = cmpure(elt, mi) & r1 = false
  with  cmp(a,b) |= {true} *->:r {r=cmpure(a,b)}
  *)
  (* [APP] eval [insert cmp elt q] with anchor [r2] 
        self::Cons<mi,q> * q::Nil & mi=ma & n=1 & r1 = cmpure(elt,mi) & r1 = false
    or  self::Cons<mi,q> * q::LLsort<n-1,k,ma> & cmpure(mi, k) = true & r1 = cmpure(elt, mi) & r1 = false
  with  cmp(a,b) |= {true} *->:r {r=cmpure(a,b)}
  *)
        (* CASE1: trivial, omitted *)
        (* CASE2: [MATCH PRE]
        
        n:=n-1, mi:=k, ma:=ma, cmpure:=cmpure
        Delta_pre  = q::LLsort<n-1,k,ma,cmpure> & cmp(a,b) |= {true} *->:r {r=cmpure(a,b)} 
        Delta_r    = self::Cons<mi,q> & cmpure(mi, k) = true & r1 = cmpure(elt, mi) & r1 = false
        Delta_post[r2]= EX left, right,
                      r2::LLsort<n,left,right,cmpure> 
                      & (cmpure(elt,k) = true <-> left = elt )
                      & (cmpure(elt,k) = false <-> left = k )
                      & (cmpure(elt,ma) = true <-> right = ma )
                      & (cmpure(elt,ma) = false <-> left = elt )
    
        *res::Cons(mi,r2) 
        
********cmpure(elt,k) ~cmpure(elt, mi) cmpure(mi, k) 
        to derive left=k / elt <= k, we need a total relation on cmpure
        
        
        |-- res::LLsort<n+1,mi,right,cmpure> * self::Cons<mi,q> & cmpure(mi, k) = true & r1 = cmpure(elt, mi) & r1 = false
            & left = k
            & (cmpure(elt,k) = true <-> left = elt )
            & (cmpure(elt,k) = false <-> left = k )
            & (cmpure(elt,ma) = true <-> right = ma )
            & (cmpure(elt,ma) = false <-> left = elt )
        --------------------------- EX left'= mi, right'=ma
        res::LLsort<n+1,left',right',cmpure> 
        & (cmpure(elt,mi) = true <-> left' = elt )
        & (cmpure(elt,mi) = false <-> left' = mi )
        & (cmpure(elt,ma) = true <-> right' = ma )
        & (cmpure(elt,ma) = false <-> left' = elt )   *)
    Cons (mi, insert cmp elt q)


