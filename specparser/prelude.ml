open Hiphop.Spectree


let div_op_sig = {
  fname="/";
  pnames=[];
  fvar=["arg1";"arg2"];
  fpre= {
    pure=[(Neg (Arith (Eq, Pvar "arg2", Const (Int 0))))];
    spec=[]
  };
  fpost="res", {pure=[(Arith (Eq, Pvar "arg1", (Op (Mult, Pvar "res", Pvar "arg2" ))))]; spec=[]};
}


let plus_sig = {
  fname="+";
  fvar=["arg1";"arg2"];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="plus_res", {pure=[(Arith (Eq, Pvar "plus_res", (Op  
      (Plus, Pvar "arg1", Pvar "arg2" )
    )))]; spec=[]};
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
  mult_sig;
  minus_sig;
  eq_sig;
  div_op_sig;
  plus_sig; (* TODO: library *)
]

let predicates = [
  
]