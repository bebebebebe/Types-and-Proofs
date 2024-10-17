(* Satisfiability *)

(* Formulas *)
type var = int

type formula =
| Var of var
| Not of formula
| And of formula * formula
| Or of formula * formula
| True
| False

(* Substitution 
A[B/X] replace variable X with formula B in a formula A
*)

let rec subst x b a = 
match a with
  | True | False -> a
  | Not a' -> Not(subst x b a')
  | And(f, g) -> And(subst x b f, subst x b g)
  | Or(f, g) -> Or(subst x b f, subst x b g)
  | Var v when v = x -> b
  | Var _ -> a

(* Free Variables. Return any free var in formula if one exists.
  Here all vars are "free variables"
  *)
let rec free_var = function
| True | False -> None
| Var v -> Some v
| Not p -> free_var p
| And(p, q) | Or(p, q) -> begin
    match free_var p with
    | Some v -> Some v
    | None -> free_var q
end

(* Define eval to evaluate closed formulas.
take a formula with no vars and returns a bool
*)

exception Not_Closed_Formula

let rec eval = function
| True -> true
| False -> false
| Not p -> not(eval p)
| And(p, q) -> (eval p) && (eval q)
| Or(p, q) -> (eval p) || (eval q)
| Var _ -> raise Not_Closed_Formula

(* Satisfiability. Determine if a formula is satisfiable.
  Does the formula have a truth value assignment that makes it true?
*)


let rec sat p = 
  match free_var p with
  | None -> eval p
  | Some v -> sat(subst v True p) || sat(subst v False p) 
