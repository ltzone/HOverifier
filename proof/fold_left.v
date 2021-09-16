Require Extraction.
Require Import ZArith.

Fixpoint fold_left {A:Type} {B:Type} (f: A -> B -> A) x ys :=
  match ys with
  | nil => x
  | cons y ys' => fold_left f (f x y) ys'
  end.

Definition id := Z.


Inductive sep_assert : Type :=.

Inductive pure_assert : Type :=.

Inductive assertion : Type :=
| assert_intro (gamma: sep_assert) (phi: pure_assert)
.