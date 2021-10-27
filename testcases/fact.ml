let rec fact n =
  if n = 0 then 1 else (n * (fact (n - 1)))

(*@
declare fact(n)
given factP(int,int)
requires { true }
ensures[res] { factP(x,res) }


pred factP(q1:int,q2:int) |=
    q1 = 0 & q2 = 1
or  q2 = r_ex * q1 & factP(q1 - 1,r_ex)

@*)
