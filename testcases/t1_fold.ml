let rec fold_left f x ys = 
  match ys with
  | [] -> x
  | y :: ys' -> fold_left f (f x y) ys'