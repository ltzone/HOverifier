open Hiphop.Spectree

let twice_sig = {
  fname="twice";
  fvar=["f";"x"];
  pnames=["fpure"];
  fpre={pure=[(True)]; spec=[
    {
      fname="f";
      fvar=["a"];
      fpre={pure=[(True)]; spec=[]};
      fpost="res0", {pure=[((Prop ("fpure", [Pvar "a"; Pvar "res0"])))]; spec=[]};
      pnames=[];
    }
  ]};
  fpost="res", {pure=[
    And (
      Prop ("fpure", [Pvar "x"; Pvar "n"]),
      Prop ("fpure", [Pvar "n"; Pvar "res"])
    )]; spec=[]};
}

let once_sig = {
  fname="once";
  fvar=["f"; "x"];
  pnames=["fpure"];
  fpre={pure=[(True)]; spec=[
    {
      fname="f";
      fvar=["a"];
      pnames=[];
      fpre={pure=[(True)]; spec=[]};
      fpost="r", {pure=[((Prop ("fpure", [Pvar "a"; Pvar "r"])))]; spec=[]}  
    }
  ]};
  fpost="p", {pure=[(((Prop ("fpure", [Pvar "x"; Pvar "p"] ))))]; spec=[]};
}


let incr_sig = {
  fname="incr";
  fvar=["x"];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="res", {pure=[(Arith (Eq, Pvar "res", (Op (Plus, Pvar "x", Const (Int 1) ))))]; spec=[]};
}

let incr_once_sig = {
  fname="incr_once";
  fvar=["x"];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="res", {pure=[(Arith (Eq, Pvar "res", (Op (Plus, Pvar "x", Const (Int 1) ))))]; spec=[]};
}

let incr_twice_sig = {
  fname="incr_twice";
  fvar=["x"];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="res", {pure=[(Arith (Eq, Pvar "res", (Op (Plus, Pvar "x", Const (Int 2) ))))]; spec=[]};
}

let double_sig = {
  fname="double";
  fvar=["x"];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="res", {pure=[(Arith (Eq, Pvar "res", (Op (Plus, Pvar "x", Pvar "x" ))))]; spec=[]};
}

let quad_sig = {
  fname="quad";
  fvar=["x"];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="res", {pure=[(Arith (Eq, Pvar "res", (Op  
     (Plus, 
     (Op (Plus, Pvar "x", Pvar "x" )), 
     (Op (Plus, Pvar "x", Pvar "x" )))
    )))]; spec=[]};
}

let quad_fo_sig = {
  fname="quad_fo";
  fvar=["x"];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="res", {pure=[(Arith (Eq, Pvar "res", (Op  
     (Plus, 
     (Op (Plus, Pvar "x", Pvar "x" )), 
     (Op (Plus, Pvar "x", Pvar "x" )))
    )))]; spec=[]};
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


let three_add_sig = {
  fname="three_add";
  fvar=["x";"y";"z"];
  pnames=[];
  fpre= {
    pure=[(True)];
    spec=[]
  };
  fpost="res", {pure=[(Arith (Eq, Pvar "res", (Op  
      (Plus, Pvar "x", Op (Plus, Pvar "z", Pvar "y" ))
    )))]; spec=[]};
}

let sigs = [
  twice_sig;
  once_sig;
  incr_sig;
  incr_once_sig;
  incr_twice_sig;
  double_sig;
  quad_sig;
  quad_fo_sig;
  three_add_sig;
  plus_sig; (* TODO: library *)
]


let incr_pure = {
  pname="incr_one";
  pargs=[("q1", Int); ("q2", Int)];
  pbody=[
    (* Arith (Eq, Pvar "x", Pvar "r") *)
    (* Should ensure no duplicate names *)
    Arith (Eq, Pvar "q2", Op (Plus, Pvar "q1", Const (Int 1)))
  ]; (* disjunctive normal form *)
}

let predicates = [
  incr_pure;
]

let pty_env = SMap.empty |> SMap.add "fpure" [Int; Int]