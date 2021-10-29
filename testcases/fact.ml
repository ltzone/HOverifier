let rec fact n =
  if n = 0 then 1 else (n * (fact (n - 1)))

(*@
declare fact(n)
given factP(int,int)
requires { 0<=n }
ensures[res] { factP(n,res) }


pred factP(q1:int,q2:int) |=
    q1 = 0 & q2 = 1
or  q2 = r_ex * q1 &   factP(q1 - 1,r_ex)
@*)


(* 

Forward:
  n>=0 & n=0 & res = 1
| n>=0 & n!=0 & (ex r', factP(n-1,r')) & res = n'*r'
----------------------------
factP(n,res)

*)