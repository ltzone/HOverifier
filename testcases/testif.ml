let test_if x =
  if x = 1 then 2
  else if x = 2 then 3
  else if x = 3 then 4
  else x+1
  (*@
  declare test_if(x)
  given 
  requires { true }
  ensures[r] { r = x + 1 }
  @*)


let test_iff x =
  if x = 1 then 2
  else if x = 2 then 3
  else if x = 3 then 4
  else test_if x
  (*@
  declare test_iff(x)
  given 
  requires { true }
  ensures[r] { r = x + 1 }
  @*)