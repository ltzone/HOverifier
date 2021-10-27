type bintree = Leaf
             | Node of int * bintree  * bintree


(**
FoldTree<init,res,fpure> = self::Leaf & init = res
                         or self::Node<v,l,r> * l::FoldTree<init,res1,fpure> * l::FoldTree<init,res2,fpure> & res=fpure(v,res1,res2)
*)


let rec foldtree init op tree =
  (* Requires       tree::FoldTree<init,r,fpure> & op(x,y,z) |= { true } *->:r { res=fpure(x,y,z) }
     Ensures[res]   tree::FoldTree<init,res,fpure>
  *)
  match tree with
  | Leaf -> init
  | Node (v,l,r) -> op v (foldtree init op l) (foldtree init op r)


let size t = foldtree 0 (fun _ l r -> 1 + l + r) t


let depth t = foldtree 0 (fun _ l r -> 1 + max l r) t


let preorder t = foldtree [] (fun x l r -> [x] @ l @ r) t