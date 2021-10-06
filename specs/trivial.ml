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

let twice_sig = {
  fname="twice";
  fvar=["f";"x"];
  fspec=twice_spec
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

let once_sig = {
  fname="once";
  fvar=["x"];
  fspec=once_spec
}

let two_arg_spec = {
  fpre={pure=[([], True)]; spec=[
    {
      fname="f";
      fvar=["a"; "b"];
      fspec={
        fpre={pure=[([], True)]; spec=[]};
        fpost="r", {pure=[([], Arith (Eq, Pvar "r", Fun ("fpure", [Pvar "a"; Pvar "b"])))]; spec=[]}
      };  
    }

  ]};
  fpost="res", {pure=[([], Arith (Eq, Pvar "res", (Fun ("fpure", [Pvar "x"; Pvar "y"] ))))]; spec=[]};
}

let two_arg_sig = {
  fname="two_arg";
  fvar=["x"; "y"];
  fspec=two_arg_spec
}

