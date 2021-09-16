type pair = { fst : int ; snd : int }

(*********** higher order functions *********)
let apply f x = f x

let pipeline x f = f x

let compose f g x = f (g x)

let both f g x : pair = {fst=(f x); snd=(g x)}

let cond p f g x =
  if p x then f x else g x

(*********** function instances *********)
let double x = x + x
(* Requires      { true }
   Ensures[res]  { res = x + x } *)

let square x = x * x
(* Requires      { true }
   Ensures[res]  { res = x * x } *)
  

(*********** test cases *********)
let ds = both double square
let p = ds 3  (* (6,9) *)
let x = pipeline 5 double  (* 10 *)