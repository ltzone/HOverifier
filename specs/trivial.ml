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
  fvar=["x"];
  pnames=["fpure"];
  fpre={pure=[(True)]; spec=[
    {
      fname="f";
      fvar=["a"];
      pnames=[];
      fpre={pure=[(True)]; spec=[]};
      fpost="r", {pure=[(Prop ("fpure", [Pvar "a";Pvar "r"]))]; spec=[]}
    }
  ]};
  fpost="p", {pure=[(( (Prop ("fpure", [Pvar "x"; Pvar "p"] ))))]; spec=[]};
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
      fpost="r", {pure=[( ( Prop ("fpure3", [Pvar "a"; Pvar "b"; Pvar "r"])))]; spec=[]}
    }

  ]};
  fpost="res", {pure=[( ((Prop ("fpure3", [Pvar "x"; Pvar "y"; Pvar "res"] ))))]; spec=[]};
}

let sigs = [
  twice_sig;
  once_sig;
  two_arg_sig; (* TODO: library *)
]

let predicates = [
  
]

let pty_env = SMap.empty