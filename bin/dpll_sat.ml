(* Satisfiability with DPLL algorithm *)

type var = int
type literal = bool * var
type clause = literal list
type cnf = clause list

(* Simple test case - example cnf *)
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
      (fun y -> list_filter (fun z -> z <> (false, x)) y)
      (list_filter (fun y -> not(list_mem (true, x) y)) p)
  else
    p (* TODO *)



