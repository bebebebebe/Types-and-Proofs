(* Satisfiability with DPLL algorithm *)

type var = int
type literal = bool * var
type clause = literal list
type cnf = clause list

(* 
A cnf [] corresponds to true
A clause [] corresponds to false
*)

(* Simple test case - example cnf symbolizes: (~x1 v ~x2) & x3 *)
let p = [[(true, 1); (false, 2)]; [(true, 3)]]

(* Define mem, map, fliter without using built in list primitives *)
let rec list_mem x = function
| [] -> false
| a :: t -> a = x || list_mem x t

let rec list_map f = function
| [] -> []
| a :: t -> (f a) :: list_map f t

let rec list_filter f = function
| [] -> []
| a :: t -> if f a then a :: list_filter f t else list_filter f t

(* Substitute T or F for a var in a statement that is in conjunctive normal form
  A[T/X] - substitute T for X in A
*)

let rec subst_cnf x is_true p =
  if is_true then
    list_map
      (fun y -> list_filter (fun z -> z <> (false, x)) y)   (* remove (false, x) literals from remaining clauses *)
      (list_filter (fun y -> not(list_mem (true, x) y)) p)  (* remove clauses with (true, x) *)
  else
    list_map
      (fun y -> list_filter (fun z -> z <> (true, x)) y)    (* remove (true, x) literals from remaining clauses *)
      (list_filter (fun y -> not(list_mem (false, x) y)) p) (* remove clauses with (false, x) *)

let rec dpll_simple p =
  match p with
  | [] -> true
  | p' when list_mem [] p' -> false
  | _ -> dpll_simple (subst_cnf (snd (List.hd(List.hd p))) true p) || dpll_simple (subst_cnf (snd (List.hd(List.hd p))) false p)

exception Not_found

(* return literal if a clause consisting of one literal exists *)
let rec unit = function
| [] -> raise Not_found
| c :: t -> if List.length(c) = 1 then List.hd(c) else unit t


(* a literal is pure if the variable always occurs with the same polarity (true or false) in every clause
  pure finds a pure literal in a formula if it exists
*)

(* is a given literal x in any clause of the cnf? *)
let rec has_literal x = function
| [] -> false
| c :: t -> list_mem x c || has_literal x t

(* remove x, not x literals from cnf *)
let remove_literals x p =
  list_map (fun c -> list_filter (fun (_, v) -> v != x) c) p

let rec pure = function
| [] -> raise Not_found
| [] :: t -> pure t
| ((b, v) :: c) :: t->
  if has_literal (not b, v) (c :: t) then pure (remove_literals v (c :: t)) (* TODO: reduce steps *)
  else (b, v)

(* test case *)
let q = [[(true, 1); (true, 2)]; [(false, 3)]]
let contradiction = [[(true, 1)]; [(false, 1)]]

(* Improve on dpll_simple above as follows:
if the formula is empty then it returns true,
if the formula contains the empty clause then it returns false,
if the formula contains a unitary clause then it replaces the corresponding variable by the only possible value,
if the formula contains a pure literal then it replaces the corresponding variable by the value which preserves satisfiability,
otherwise, it splits on an arbitrary variable.
*)

let rec dpll p =
  match p with
  | [] -> true
  | p' when list_mem [] p' -> false
  | _ ->
    try (match unit p with
    | (true, v) -> dpll (subst_cnf v true p)
    | (false, v) -> dpll (subst_cnf v false p))
  with Not_found ->
    try (match pure p with
    | (true, v) -> dpll (subst_cnf v true p)
    | (false, v) -> dpll (subst_cnf v false p))
  with Not_found ->
    dpll (subst_cnf (snd (List.hd(List.hd p))) true p) || dpll (subst_cnf (snd (List.hd(List.hd p))) false p)

(* constructive version of dpll --> return an assignment if p is satisfiable *)
  let rec dpll_con p acc =
    match p with
    | [] -> (true, acc)
    | p' when list_mem [] p' -> (false, acc)
    | _ ->
      try (match unit p with
      | (true, v) -> dpll_con (subst_cnf v true p) ((true, v) :: acc)
      | (false, v) -> dpll_con (subst_cnf v false p) ((false, v) :: acc))
    with Not_found ->
      try (match pure p with
      | (true, v) -> dpll_con (subst_cnf v true p) ((true, v) :: acc)
      | (false, v) -> dpll_con (subst_cnf v false p) ((false, v) :: acc))
    with Not_found ->
      let v = (snd (List.hd(List.hd p))) in (
        match (dpll_con (subst_cnf v true p) ((true, v) :: acc)) with
        | (true, q) -> (true, q)
        | (false, q) -> match (dpll_con (subst_cnf v false p) ((false, v) :: acc)) with 
          | (true, r) -> (true, r)
          | (false, r) -> (false, r)
      )
