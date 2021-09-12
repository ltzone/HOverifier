(* basic term *)
type basic_t = BINT of int | UNIT 

type instant = string * (basic_t list) 

(* To indicate weather there are partial applied continustiaon *)
type residue = instant option 

type event = One of string | Zero of string | Pred of instant | Any

(*
type stack_content = Cons of int | Variable of string | Apply of string * stack_content list 
*)

type stack = string * instant

type es = Bot 
        | Emp   
        | Predicate of instant (* Q () *)
        | Event of string
        | Not of string
        | Cons of es * es
        | ESOr of es * es
        | Kleene of es (* 0 or more, possibly infinite*)
        | Omega of es(* infinite*)
        | Underline

type term = 
      Num of int
    | Var of string
    | Plus of term * term 
    | Minus of term * term 

type bin_op = GT | LT | EQ | GTEQ | LTEQ

type pi = 
  | True
  | False
  | Atomic of bin_op * term * term
  | And    of pi * pi
  | Or     of pi * pi
  | Imply  of pi * pi
  | Not    of pi

type side = (string * (es * es)) list   (* Eff(f()) = _^*.A -> U^*.(Res \/ emp) *)

(* e.g: spec =  (n>0, emp, [Eff(f() = U^*] )) 
                (n>0, Q(Foo()).END, [] )) 
*)
type spec = pi * es * side

let default_spec = True, Emp, []

type policy = Eff of string * es | Exn of string

type evn = (es * es)list