let ho_fun f x y = f x y


let f = ho_fun (+) (print_endline "eval_1"; 1 + (print_endline "evaling args"; 1))

let w = f (print_endline "eval_2"; 2)