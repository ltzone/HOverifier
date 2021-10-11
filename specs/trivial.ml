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
      fpost="res0", {pure=[(Arith (Eq, Pvar "res0", Fun ("fpure", [Pvar "a"])))]; spec=[]};
      pnames=[];
    }
  ]};
  fpost="res", {pure=[(Arith (Eq, Pvar "res", (Fun ("fpure", [ Fun ("fpure", [Pvar "x"]) ] ))))]; spec=[]};
}



let once_sig = {
  fname="once";
  fvar=["x"];
  pnames=["fpure"];
  fpre={pure=[(True)]; spec=[
    {
      fname="f";
      fvar=["a"];
      pnames=[];
      fpre={pure=[(True)]; spec=[]};
      fpost="r", {pure=[(Arith (Eq, Pvar "r", Fun ("fpure", [Pvar "a"])))]; spec=[]}
    }
  ]};
  fpost="p", {pure=[(Arith (Eq, Pvar "p", (Fun ("fpure", [Pvar "x"] ))))]; spec=[]};
}

let two_arg_sig = {
  fname="two_arg";
  fvar=["x"; "y"];
  pnames=["fpure"];
  fpre={pure=[(True)]; spec=[
    {
      fname="f";
      fvar=["a"; "b"];
      pnames=[];
      fpre={pure=[(True)]; spec=[]};
      fpost="r", {pure=[(Arith (Eq, Pvar "r", Fun ("fpure", [Pvar "a"; Pvar "b"])))]; spec=[]}
    }

  ]};
  fpost="res", {pure=[(Arith (Eq, Pvar "res", (Fun ("fpure", [Pvar "x"; Pvar "y"] ))))]; spec=[]};
}

let sigs = [
  twice_sig;
  once_sig;
  two_arg_sig; (* TODO: library *)
]