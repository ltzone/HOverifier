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

let div_sig = {
  fname="div";
  pnames=[];
  fvar=["y";"x"];
  fpre= {
    pure=[(Neg (Arith (Eq, Pvar "y", Const (Int 0))))];
    spec=[]
  };
  fpost="dres", {pure=[(Arith (Eq, Pvar "x", (Op (Mult, Pvar "dres", Pvar "y" ))))]; spec=[]};
}



let div_by_one_sig = {
  fname="div_by_one";
  fvar=[];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="d1res", {pure=[True]; spec=[
    {
      fname="d1res";
      fvar=["x"];
      pnames=[];
      fpre={
        pure=[(True)];
        spec=[]
      };
      fpost="m", {pure=[(Arith (Eq, Pvar "m", Pvar "x"))];spec=[]}
    }]};
}

let div_by_one'_sig = {
  fname="div_by_one'";
  fvar=["x"];
  pnames=[];
  fpre={
    pure=[(True)];
    spec=[]
  };
  fpost="m", {pure=[(Arith (Eq, Pvar "m", Pvar "x"))];spec=[]}
}

let twice_sig = {
  fname="twice";
  fvar=["f";"x"];
  pnames=[];
  fpre={pure=[(True)]; spec=[
    {
      fname="f";
      fvar=["a"];
      pnames=[];
        fpre={pure=[(True)]; spec=[]};
        fpost="res0", {pure=[(Prop ("fpure", [Pvar "a"; Pvar "res0"]))]; spec=[]}
    }
  ]};
  fpost="res", {pure=[(And (
        (And (Prop ("fpure", [Pvar "x"; Pvar "r"]),
                            Prop ("fpure", [Pvar "r"; Pvar "res"])))
                            ,
              (* Prop ("fpure", [Pvar "x"; Pvar "res"])) *)
                          True)
                            )]; spec=[]};
}



let div_by_one_star_sig = {
  fname="div_by_one_star";
  fvar=["f";"x"];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="res", {pure=[(Arith (Eq, Pvar "x", Pvar "res"))]; spec=[]};
}

let sigs = [
  div_sig;
  div_op_sig;
  div_by_one_sig;
  twice_sig;
  div_by_one_star_sig;
  div_by_one'_sig ;
]

let div_one_pure = {
  pname="div_one";
  pargs=[("q1", Int); ("q2", Int)];
  pbody=[
    (* Arith (Eq, Pvar "x", Pvar "r") *)
    (* Should ensure no duplicate names *)
    Arith (Eq, Pvar "q1", Pvar "q2")
  ]; (* disjunctive normal form *)
}

let predicates = [
  div_one_pure;
]

let pty_env = SMap.empty |> SMap.add "fpure" [Int; Int]