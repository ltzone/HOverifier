let rec fib n =
  if n = 0 then 1 else
    if n = 1 then 1 else 
      fib (n - 1) + fib (n - 2)

(*@
declare fib(n)
given fibP(int,int)
requires { 0<=n }
ensures[res] { fibP(n,res) }


pred fibP(q1:int,q2:int) |=
    q1 = 0 & q2 = 1
or  q1 = 1 & q2 = 1
or  EX (r1:int) (r2:int), q2 = r1 + r2 & fibP(q1 - 1,r1) & fibP(q1 - 2,r2)

@*)
