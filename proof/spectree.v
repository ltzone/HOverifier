Require Import Coq.ZArith.ZArith.
Require Import Coq.Strings.String.
Require Import Coq.Lists.List.
Local Open Scope Z.
Local Open Scope string.
Local Open Scope list.


Inductive var: Type := Var (v: string).
Coercion Var : string >-> var.

Inductive logical_var: Type := Logical_var (v: string).
Coercion Logical_var : string >-> logical_var.

Inductive op : Type :=
  | Oplus
  | Ominus
  | Omult
  | Oeq
  | Ole
  | Onot
  | Oand
  | Oifthenelse.

Inductive constant : Type :=
  | int_const (n: Z): constant
  | bool_const (b: bool): constant
  | op_const (o: op): constant.

Inductive Assertion : Type :=
  | DLe (t1 t2 : term)
  | DLt (t1 t2 : term)
  | DEq (t1 t2 : term)
  | DInj (b: bexp)
  | DProp (P: Prop)
  | DOr (d1 d2 : Assertion)
  | DAnd (d1 d2 : Assertion)
  | DNot (d: Assertion)
  | DExists (d: Z -> Assertion)
  | DForall (d: Z -> Assertion).


Inductive sep_assert : Type :=.

Inductive pure_assert : Type :=.

Inductive assertion : Type :=
| assert_intro (gamma: sep_assert) (phi: pure_assert)
.