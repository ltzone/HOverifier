open Hiphop.Spectree
 


let fact_sig = {
  fname="fact";
  fvar=["n"];
  pnames=[];
  fpre={pure=[(True)]; spec=[]};
  fpost="res", {pure=[
    Prop ("factP", [Pvar "x"; Pvar "res"])
  ];
  spec=[]
  } 
  (* fpost="q2", {
    pure=[
      And (Arith (Eq, Pvar "n", Const (Int 0)), Arith (Eq, Pvar "q2", Const (Int 1))) ;
      And (Arith (Eq, Pvar "q2", Op (Mult, Pvar "r", Pvar "n")), Prop ("factP", [Op (Minus, Pvar "n", Const (Int 1)); Pvar "r"]))    
    ];
    spec=[]
  }; *)
}

let eq_sig = {
  fname="=";
  fvar=["p1"; "p2"];
  pnames=[];
  fpre={pure=[(True)]; spec=[]};
  fpost="r", {pure=[
    And ( Arith (Eq, Pvar "r", Const (Int 1)), Arith (Eq, Pvar "p1", Pvar "p2"));
    And ( Arith (Eq, Pvar "r", Const (Int 0)), Neg (Arith (Eq, Pvar "p1", Pvar "p2")))
  ]; spec=[]};
}

let minus_sig = {
  fname="-";
  fvar=["w1"; "w2"];
  pnames=[];
  fpre={pure=[(True)]; spec=[]};
  fpost="res0", {pure=[
    Arith (Eq, Pvar "res0", Op (Minus, Pvar "w1", Pvar "w2"))
  ]; spec=[]};
}

let mult_sig = {
  fname="*";
  fvar=["y1"; "y2"];
  pnames=[];
  fpre={pure=[(True)]; spec=[]};
  fpost="res1", {pure=[
    Arith (Eq, Pvar "res1", Op (Mult, Pvar "y1", Pvar "y2"))
  ]; spec=[]};
}

let sigs = [
  fact_sig;
  eq_sig;
  minus_sig;
  mult_sig;
]


let double_pure = {
  pname="factP";
  pargs=[("q1", Int); ("q2", Int)];
  pbody=[
    (* Arith (Eq, Pvar "x", Pvar "r") *)
    (* Should ensure no duplicate names *)
    And (Arith (Eq, Pvar "q1", Const (Int 0)), Arith (Eq, Pvar "q2", Const (Int 1))) ;
    And (Arith (Eq, Pvar "q2", Op (Mult, Pvar "r_ex", Pvar "q1")), Prop ("factP", [Op (Minus, Pvar "q1", Const (Int 1)); Pvar "r_ex"]))
  ]; (* disjunctive normal form *)
}


let predicates = [
  (* double_pure *)
]

let pty_env = SMap.empty 
(* |> SMap.add "factP" [Int; Int]  *)