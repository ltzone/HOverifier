let div y x = 
      (x / y)

(*@
declare div(y, x)
requires    { ~ y = 0 }
ensures  [r]{ r * y = x }
@*)

let div_by_one = (div 1)
(*@
declare div_by_one()
requires    { true }
ensures  [f]{ true with 
                f(x) |= { true } *->:m { m * 1 = x } }
@*)


let div_by_one' x = div 1 x
(*@
declare div_by_one'(x)
requires    { true } 
ensures  [m]{ m * 1 = x }
@*)

let twice f x = f (f x)
(*@
declare twice(f,x)
given Q(int, int)
requires    { true with
                f(x) |= {true} *->:r {Q(x,r)} }
ensures  [r]{EX (m:int), Q(x,m) & Q(m,r) }
@*)



let div_by_one_star x = twice div_by_one x
(*@
declare div_by_one_star(x)
requires    { true }
ensures  [r]{ r=x }

// a candidate predicate for twice to i
pred div_one(q1:int,q2:int) |= 
 q1 = q2
@*)