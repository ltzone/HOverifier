declare twice(f,x)
given fpure(int,int)
requires { true with
    f(a) |= { true } *->:res0 {fpure(a,res0)} }
ensures[res] { fpure(x,n) & fpure(n,res) }

declare once(f,x)
given fpure2(int,int)
requires { true with
   f(a) |= { true } *->:r { fpure2(a,r)}  }
ensures[p] { fpure2(x,p) }

declare two_arg(x,y)
given fpure3(int,int,int)
requires { true with
  f(a,b) |= {true} *->:r {fpure3(a,b,r)} }
ensures[res] { fpure3(x,y,res) }


