open Hiphop.Spectree

let foo_sig = {
  fname="foo";
  fvar=["x";"y"];
  pnames=[];
  fpre={pure=[(True)]; spec=[]};
  fpost="res", {pure=[
    Arith (Eq, Pvar "res",
     (Op (Minus, Pvar "x", Pvar "y"))
    )
  ]; spec=[]};
}

let bar_sig = {
  fname="bar";
  fvar=["f"; "a"; "b"];
  pnames=[];
  fpre={
    pure=[(Arith (Le, Const (Int 0), Pvar "b"))]
    ; spec=[
    {
      fname="f";
      fvar=["m"; "n"];
      pnames=[];
      fpre={pure=[(Arith (Le, Const (Int 0), Pvar "n"))]; spec=[]};
      fpost="res0", {pure=[ Arith (Le, Pvar "res0", Pvar "m") ]; spec=[]}  
    }
  ]};
  fpost="p", {pure=[ Arith (Le, Pvar "p", Pvar "a")]; spec=[]};
}


let baz_sig = {
  fname="baz";
  fvar=["c"; "d"];
  pnames=[];
  fpre= {
    pure=[
    (Arith (Le, Const (Int 0), Pvar "d"))]; 
    spec=[]
  };
  fpost="r", {pure=[ Arith (Le, Pvar "r", Pvar "c")]; spec=[]};
}

let minus_sig = {
  fname="-";
  fvar=["arg1";"arg2"];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="minus_res", {pure=[(Arith (Eq, Pvar "minus_res", (Op  
      (Minus, Pvar "arg1", Pvar "arg2" )
    )))]; spec=[]};
}

let sigs = [
  foo_sig;
  bar_sig;
  baz_sig;
  minus_sig;
]



let predicates = [
]

let pty_env = SMap.empty 