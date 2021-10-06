open Hiphop.Spectree


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



let once_spec = {
  fpre={pure=[([], True)]; spec=[
    {
      fname="f";
      fvar=["a"];
      fspec={
        fpre={pure=[([], True)]; spec=[]};
        fpost="r", {pure=[([], Arith (Eq, Pvar "r", Fun ("fpure", [Pvar "a"])))]; spec=[]}
      };  
    }
  ]};
  fpost="p", {pure=[([], Arith (Eq, Pvar "p", (Fun ("fpure", [Pvar "x"] ))))]; spec=[]};
}

let incr_spec = {
  fpre= {
    pure=[([], True)];
    spec=[]
  };
  fpost="res", {pure=[([], Arith (Eq, Pvar "res", (Op (Plus, Pvar "x", Const (Int 1) ))))]; spec=[]};
}

let incr_twice_spec = {
  fpre= {
    pure=[([], True)];
    spec=[]
  };
  fpost="res", {pure=[([], Arith (Eq, Pvar "res", (Op (Plus, Pvar "x", Const (Int 2) ))))]; spec=[]};
}

let double_spec = {
  fpre= {
    pure=[([], True)];
    spec=[]
  };
  fpost="res", {pure=[([], Arith (Eq, Pvar "res", (Op (Plus, Pvar "x", Pvar "x" ))))]; spec=[]};
}

let quad_spec = {
  fpre= {
    pure=[([], True)];
    spec=[]
  };
  fpost="res", {pure=[([], Arith (Eq, Pvar "res", (Op  
     (Plus, 
     (Op (Plus, Pvar "x", Pvar "x" )), 
     (Op (Plus, Pvar "x", Pvar "x" )))
    )))]; spec=[]};
}


let twice_sig = {
  fname="twice";
  fvar=["f";"x"];
  fspec=twice_spec
}

let once_sig = {
  fname="once";
  fvar=["f"; "x"];
  fspec=once_spec
}


let incr_sig = {
  fname="incr";
  fvar=["x"];
  fspec=incr_spec
}

let incr_once_sig = {
  fname="incr_once";
  fvar=["x"];
  fspec=incr_spec
}

let incr_twice_sig = {
  fname="incr_twice";
  fvar=["x"];
  fspec=incr_twice_spec
}

let double_sig = {
  fname="double";
  fvar=["x"];
  fspec=double_spec
}

let quad_sig = {
  fname="quad";
  fvar=["x"];
  fspec=quad_spec
}

let quad_fo_sig = {
  fname="quad_fo";
  fvar=["x"];
  fspec=quad_spec
}

let plus_sig = {
  fname="+";
  fvar=["arg1";"arg2"];
  fspec={
    fpre= {
      pure=[([], True)];
      spec=[]
    };
    fpost="plus_res", {pure=[([], Arith (Eq, Pvar "plus_res", (Op  
       (Plus, Pvar "arg1", Pvar "arg2" )
      )))]; spec=[]};
  }
}


let three_add_sig = {
  fname="three_add";
  fvar=["x";"y";"z"];
  fspec={
    fpre= {
      pure=[([], True)];
      spec=[]
    };
    fpost="res", {pure=[([], Arith (Eq, Pvar "res", (Op  
       (Plus, Pvar "x", Op (Plus, Pvar "z", Pvar "y" ))
      )))]; spec=[]};
  }
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