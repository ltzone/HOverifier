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