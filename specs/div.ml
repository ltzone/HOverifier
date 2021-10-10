open Hiphop.Spectree

let div_op_spec = {
  fpre= {
    pure=[([], Neg (Arith (Eq, Pvar "arg2", Const (Int 0))))];
    spec=[]
  };
  fpost="res", {pure=[([], Arith (Eq, Pvar "arg1", (Op (Mult, Pvar "res", Pvar "arg2" ))))]; spec=[]};
}

let div_op_sig = {
  fname="/";
  fvar=["arg1";"arg2"];
  fspec=div_op_spec
}

let div_spec = {
  fpre= {
    pure=[([], Neg (Arith (Eq, Pvar "y", Const (Int 0))))];
    spec=[]
  };
  fpost="dres", {pure=[([], Arith (Eq, Pvar "x", (Op (Mult, Pvar "dres", Pvar "y" ))))]; spec=[]};
}

let div_sig = {
  fname="div";
  fvar=["y";"x"];
  fspec=div_spec
}


let div_by_one_spec = {
  fpre= {
    pure=[([], True)];
    spec=[]
  };
  fpost="d1res", {pure=[([],True)]; spec=[
    {
      fname="d1res";
      fvar=["x"];
      fspec={
        fpre={
          pure=[([], True)];
          spec=[]
        };
        fpost="m", {pure=[([], Arith (Eq, Pvar "m", Pvar "x"))];spec=[]}
      }
    }]};
}

let div_by_one_sig = {
  fname="div_by_one";
  fvar=[];
  fspec=div_by_one_spec
}


let twice_spec = {
  fpre={pure=[([], True)]; spec=[
    {
      fname="f";
      fvar=["a"];
      fspec={
        fpre={pure=[([], True)]; spec=[]};
        fpost="res0", {pure=[([], Arith (Eq, Pvar "res0", Fun ("fpure", [Pvar "a"])))]; spec=[]}
      };  
    }
  ]};
  fpost="res", {pure=[([], Arith (Eq, Pvar "res", (Fun ("fpure", [ Fun ("fpure", [Pvar "x"]) ] ))))]; spec=[]};
}

let twice_sig = {
  fname="twice";
  fvar=["f";"x"];
  fspec=twice_spec
}


let div_by_one_star_spec = {
  fpre= {
    pure=[([], True)];
    spec=[]
  };
  fpost="res", {pure=[([], Arith (Eq, Pvar "x", Pvar "res"))]; spec=[]};
}

let div_by_one_star_sig = {
  fname="div_by_one_star";
  fvar=["f";"x"];
  fspec=div_by_one_star_spec
}

let sigs = [
  div_sig;
  div_op_sig;
  div_by_one_sig;
  twice_sig;
  div_by_one_star_sig;
]