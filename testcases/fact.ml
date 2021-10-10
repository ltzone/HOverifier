let rec fact n =
  if n = 0 then 1 else (n * (fact (n - 1)))

(* FactP(n,res) == n = 0 /\ res = 1
                or FactP(n-1, r) /\ res=r*n *)

(* 
   Requires     { true }
   Ensures:res  { FactP(n,res) }
*)
