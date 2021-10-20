open Hiphop.Spectree

let fact_sig = {
  fname="fact";
  fvar=["n"];
  pnames=[];
  fpre={pure=[(True)]; spec=[]};
  fpost="res", {pure=[
    Prop ("factP", [Pvar "x"; Pvar "res"])
  ]; spec=[]};
}

let sigs = [
  fact_sig;
]


let double_pure = {
  pname="factP";
  pargs=[("q1", Int); ("q2", Int)];
  pbody=[
    (* Arith (Eq, Pvar "x", Pvar "r") *)
    (* Should ensure no duplicate names *)
    And (Arith (Eq, Pvar "q1", Const (Int 0)), Arith (Eq, Pvar "q2", Const (Int 1))) ;
    And (Arith (Eq, Pvar "q2", Op (Mult, Pvar "r", Pvar "n")), Prop ("factP", [Op (Minus, Pvar "q1", Const (Int 1)); Pvar "r"]))
  ]; (* disjunctive normal form *)
}


let predicates = [
  double_pure
]

let pty_env = SMap.empty 
|> SMap.add "factP" [Int; Int] 