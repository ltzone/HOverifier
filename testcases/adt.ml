type list = Nil
| Cons of (int * list)

let foo x = match x with
| Nil -> 0
| Cons (y, ys') -> 0


(*@
declare foo(x:list)
given 
requires { true }
ensures[r] { r=0 }
@*)